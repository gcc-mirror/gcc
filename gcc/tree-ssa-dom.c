/* SSA Dominator optimizations for trees
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hash-table.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "function.h"
#include "gimple-pretty-print.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "domwalk.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-threadupdate.h"
#include "langhooks.h"
#include "params.h"
#include "tree-ssa-threadedge.h"
#include "tree-ssa-dom.h"

/* This file implements optimizations on the dominator tree.  */

/* Representation of a "naked" right-hand-side expression, to be used
   in recording available expressions in the expression hash table.  */

enum expr_kind
{
  EXPR_SINGLE,
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_TERNARY,
  EXPR_CALL,
  EXPR_PHI
};

struct hashable_expr
{
  tree type;
  enum expr_kind kind;
  union {
    struct { tree rhs; } single;
    struct { enum tree_code op;  tree opnd; } unary;
    struct { enum tree_code op;  tree opnd0, opnd1; } binary;
    struct { enum tree_code op;  tree opnd0, opnd1, opnd2; } ternary;
    struct { gimple fn_from; bool pure; size_t nargs; tree *args; } call;
    struct { size_t nargs; tree *args; } phi;
  } ops;
};

/* Structure for recording known values of a conditional expression
   at the exits from its block.  */

typedef struct cond_equivalence_s
{
  struct hashable_expr cond;
  tree value;
} cond_equivalence;


/* Structure for recording edge equivalences as well as any pending
   edge redirections during the dominator optimizer.

   Computing and storing the edge equivalences instead of creating
   them on-demand can save significant amounts of time, particularly
   for pathological cases involving switch statements.

   These structures live for a single iteration of the dominator
   optimizer in the edge's AUX field.  At the end of an iteration we
   free each of these structures and update the AUX field to point
   to any requested redirection target (the code for updating the
   CFG and SSA graph for edge redirection expects redirection edge
   targets to be in the AUX field for each edge.  */

struct edge_info
{
  /* If this edge creates a simple equivalence, the LHS and RHS of
     the equivalence will be stored here.  */
  tree lhs;
  tree rhs;

  /* Traversing an edge may also indicate one or more particular conditions
     are true or false.  */
  vec<cond_equivalence> cond_equivalences;
};

/* Stack of available expressions in AVAIL_EXPRs.  Each block pushes any
   expressions it enters into the hash table along with a marker entry
   (null).  When we finish processing the block, we pop off entries and
   remove the expressions from the global hash table until we hit the
   marker.  */
typedef struct expr_hash_elt * expr_hash_elt_t;

static vec<expr_hash_elt_t> avail_exprs_stack;

/* Structure for entries in the expression hash table.  */

struct expr_hash_elt
{
  /* The value (lhs) of this expression.  */
  tree lhs;

  /* The expression (rhs) we want to record.  */
  struct hashable_expr expr;

  /* The stmt pointer if this element corresponds to a statement.  */
  gimple stmt;

  /* The hash value for RHS.  */
  hashval_t hash;

  /* A unique stamp, typically the address of the hash
     element itself, used in removing entries from the table.  */
  struct expr_hash_elt *stamp;
};

/* Hashtable helpers.  */

static bool hashable_expr_equal_p (const struct hashable_expr *,
				   const struct hashable_expr *);
static void free_expr_hash_elt (void *);

struct expr_elt_hasher
{
  typedef expr_hash_elt value_type;
  typedef expr_hash_elt compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

inline hashval_t
expr_elt_hasher::hash (const value_type *p)
{
  return p->hash;
}

inline bool
expr_elt_hasher::equal (const value_type *p1, const compare_type *p2)
{
  gimple stmt1 = p1->stmt;
  const struct hashable_expr *expr1 = &p1->expr;
  const struct expr_hash_elt *stamp1 = p1->stamp;
  gimple stmt2 = p2->stmt;
  const struct hashable_expr *expr2 = &p2->expr;
  const struct expr_hash_elt *stamp2 = p2->stamp;

  /* This case should apply only when removing entries from the table.  */
  if (stamp1 == stamp2)
    return true;

  /* FIXME tuples:
     We add stmts to a hash table and them modify them. To detect the case
     that we modify a stmt and then search for it, we assume that the hash
     is always modified by that change.
     We have to fully check why this doesn't happen on trunk or rewrite
     this in a more  reliable (and easier to understand) way. */
  if (((const struct expr_hash_elt *)p1)->hash
      != ((const struct expr_hash_elt *)p2)->hash)
    return false;

  /* In case of a collision, both RHS have to be identical and have the
     same VUSE operands.  */
  if (hashable_expr_equal_p (expr1, expr2)
      && types_compatible_p (expr1->type, expr2->type))
    {
      /* Note that STMT1 and/or STMT2 may be NULL.  */
      return ((stmt1 ? gimple_vuse (stmt1) : NULL_TREE)
	      == (stmt2 ? gimple_vuse (stmt2) : NULL_TREE));
    }

  return false;
}

/* Delete an expr_hash_elt and reclaim its storage.  */

inline void
expr_elt_hasher::remove (value_type *element)
{
  free_expr_hash_elt (element);
}

/* Hash table with expressions made available during the renaming process.
   When an assignment of the form X_i = EXPR is found, the statement is
   stored in this table.  If the same expression EXPR is later found on the
   RHS of another statement, it is replaced with X_i (thus performing
   global redundancy elimination).  Similarly as we pass through conditionals
   we record the conditional itself as having either a true or false value
   in this table.  */
static hash_table <expr_elt_hasher> avail_exprs;

/* Stack of dest,src pairs that need to be restored during finalization.

   A NULL entry is used to mark the end of pairs which need to be
   restored during finalization of this block.  */
static vec<tree> const_and_copies_stack;

/* Track whether or not we have changed the control flow graph.  */
static bool cfg_altered;

/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;

/* Statistics for dominator optimizations.  */
struct opt_stats_d
{
  long num_stmts;
  long num_exprs_considered;
  long num_re;
  long num_const_prop;
  long num_copy_prop;
};

static struct opt_stats_d opt_stats;

/* Local functions.  */
static void optimize_stmt (basic_block, gimple_stmt_iterator);
static tree lookup_avail_expr (gimple, bool);
static hashval_t avail_expr_hash (const void *);
static void htab_statistics (FILE *, hash_table <expr_elt_hasher>);
static void record_cond (cond_equivalence *);
static void record_const_or_copy (tree, tree);
static void record_equality (tree, tree);
static void record_equivalences_from_phis (basic_block);
static void record_equivalences_from_incoming_edge (basic_block);
static void eliminate_redundant_computations (gimple_stmt_iterator *);
static void record_equivalences_from_stmt (gimple, int);
static void remove_local_expressions_from_table (void);
static void restore_vars_to_original_value (void);
static edge single_incoming_edge_ignoring_loop_edges (basic_block);


/* Given a statement STMT, initialize the hash table element pointed to
   by ELEMENT.  */

static void
initialize_hash_element (gimple stmt, tree lhs,
                         struct expr_hash_elt *element)
{
  enum gimple_code code = gimple_code (stmt);
  struct hashable_expr *expr = &element->expr;

  if (code == GIMPLE_ASSIGN)
    {
      enum tree_code subcode = gimple_assign_rhs_code (stmt);

      switch (get_gimple_rhs_class (subcode))
        {
        case GIMPLE_SINGLE_RHS:
	  expr->kind = EXPR_SINGLE;
	  expr->type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	  expr->ops.single.rhs = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_UNARY_RHS:
	  expr->kind = EXPR_UNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.unary.op = subcode;
	  expr->ops.unary.opnd = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_BINARY_RHS:
	  expr->kind = EXPR_BINARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.binary.op = subcode;
	  expr->ops.binary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.binary.opnd1 = gimple_assign_rhs2 (stmt);
	  break;
        case GIMPLE_TERNARY_RHS:
	  expr->kind = EXPR_TERNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.ternary.op = subcode;
	  expr->ops.ternary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.ternary.opnd1 = gimple_assign_rhs2 (stmt);
	  expr->ops.ternary.opnd2 = gimple_assign_rhs3 (stmt);
	  break;
        default:
          gcc_unreachable ();
        }
    }
  else if (code == GIMPLE_COND)
    {
      expr->type = boolean_type_node;
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = gimple_cond_code (stmt);
      expr->ops.binary.opnd0 = gimple_cond_lhs (stmt);
      expr->ops.binary.opnd1 = gimple_cond_rhs (stmt);
    }
  else if (code == GIMPLE_CALL)
    {
      size_t nargs = gimple_call_num_args (stmt);
      size_t i;

      gcc_assert (gimple_call_lhs (stmt));

      expr->type = TREE_TYPE (gimple_call_lhs (stmt));
      expr->kind = EXPR_CALL;
      expr->ops.call.fn_from = stmt;

      if (gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE))
        expr->ops.call.pure = true;
      else
        expr->ops.call.pure = false;

      expr->ops.call.nargs = nargs;
      expr->ops.call.args = XCNEWVEC (tree, nargs);
      for (i = 0; i < nargs; i++)
        expr->ops.call.args[i] = gimple_call_arg (stmt, i);
    }
  else if (code == GIMPLE_SWITCH)
    {
      expr->type = TREE_TYPE (gimple_switch_index (stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_switch_index (stmt);
    }
  else if (code == GIMPLE_GOTO)
    {
      expr->type = TREE_TYPE (gimple_goto_dest (stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_goto_dest (stmt);
    }
  else if (code == GIMPLE_PHI)
    {
      size_t nargs = gimple_phi_num_args (stmt);
      size_t i;

      expr->type = TREE_TYPE (gimple_phi_result (stmt));
      expr->kind = EXPR_PHI;
      expr->ops.phi.nargs = nargs;
      expr->ops.phi.args = XCNEWVEC (tree, nargs);

      for (i = 0; i < nargs; i++)
        expr->ops.phi.args[i] = gimple_phi_arg_def (stmt, i);
    }
  else
    gcc_unreachable ();

  element->lhs = lhs;
  element->stmt = stmt;
  element->hash = avail_expr_hash (element);
  element->stamp = element;
}

/* Given a conditional expression COND as a tree, initialize
   a hashable_expr expression EXPR.  The conditional must be a
   comparison or logical negation.  A constant or a variable is
   not permitted.  */

static void
initialize_expr_from_cond (tree cond, struct hashable_expr *expr)
{
  expr->type = boolean_type_node;

  if (COMPARISON_CLASS_P (cond))
    {
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = TREE_CODE (cond);
      expr->ops.binary.opnd0 = TREE_OPERAND (cond, 0);
      expr->ops.binary.opnd1 = TREE_OPERAND (cond, 1);
    }
  else if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
    {
      expr->kind = EXPR_UNARY;
      expr->ops.unary.op = TRUTH_NOT_EXPR;
      expr->ops.unary.opnd = TREE_OPERAND (cond, 0);
    }
  else
    gcc_unreachable ();
}

/* Given a hashable_expr expression EXPR and an LHS,
   initialize the hash table element pointed to by ELEMENT.  */

static void
initialize_hash_element_from_expr (struct hashable_expr *expr,
                                   tree lhs,
                                   struct expr_hash_elt *element)
{
  element->expr = *expr;
  element->lhs = lhs;
  element->stmt = NULL;
  element->hash = avail_expr_hash (element);
  element->stamp = element;
}

/* Compare two hashable_expr structures for equivalence.
   They are considered equivalent when the the expressions
   they denote must necessarily be equal.  The logic is intended
   to follow that of operand_equal_p in fold-const.c  */

static bool
hashable_expr_equal_p (const struct hashable_expr *expr0,
                        const struct hashable_expr *expr1)
{
  tree type0 = expr0->type;
  tree type1 = expr1->type;

  /* If either type is NULL, there is nothing to check.  */
  if ((type0 == NULL_TREE) ^ (type1 == NULL_TREE))
    return false;

  /* If both types don't have the same signedness, precision, and mode,
     then we can't consider  them equal.  */
  if (type0 != type1
      && (TREE_CODE (type0) == ERROR_MARK
	  || TREE_CODE (type1) == ERROR_MARK
	  || TYPE_UNSIGNED (type0) != TYPE_UNSIGNED (type1)
	  || TYPE_PRECISION (type0) != TYPE_PRECISION (type1)
	  || TYPE_MODE (type0) != TYPE_MODE (type1)))
    return false;

  if (expr0->kind != expr1->kind)
    return false;

  switch (expr0->kind)
    {
    case EXPR_SINGLE:
      return operand_equal_p (expr0->ops.single.rhs,
                              expr1->ops.single.rhs, 0);

    case EXPR_UNARY:
      if (expr0->ops.unary.op != expr1->ops.unary.op)
        return false;

      if ((CONVERT_EXPR_CODE_P (expr0->ops.unary.op)
           || expr0->ops.unary.op == NON_LVALUE_EXPR)
          && TYPE_UNSIGNED (expr0->type) != TYPE_UNSIGNED (expr1->type))
        return false;

      return operand_equal_p (expr0->ops.unary.opnd,
                              expr1->ops.unary.opnd, 0);

    case EXPR_BINARY:
      if (expr0->ops.binary.op != expr1->ops.binary.op)
	return false;

      if (operand_equal_p (expr0->ops.binary.opnd0,
			   expr1->ops.binary.opnd0, 0)
	  && operand_equal_p (expr0->ops.binary.opnd1,
			      expr1->ops.binary.opnd1, 0))
	return true;

      /* For commutative ops, allow the other order.  */
      return (commutative_tree_code (expr0->ops.binary.op)
	      && operand_equal_p (expr0->ops.binary.opnd0,
				  expr1->ops.binary.opnd1, 0)
	      && operand_equal_p (expr0->ops.binary.opnd1,
				  expr1->ops.binary.opnd0, 0));

    case EXPR_TERNARY:
      if (expr0->ops.ternary.op != expr1->ops.ternary.op
	  || !operand_equal_p (expr0->ops.ternary.opnd2,
			       expr1->ops.ternary.opnd2, 0))
	return false;

      if (operand_equal_p (expr0->ops.ternary.opnd0,
			   expr1->ops.ternary.opnd0, 0)
	  && operand_equal_p (expr0->ops.ternary.opnd1,
			      expr1->ops.ternary.opnd1, 0))
	return true;

      /* For commutative ops, allow the other order.  */
      return (commutative_ternary_tree_code (expr0->ops.ternary.op)
	      && operand_equal_p (expr0->ops.ternary.opnd0,
				  expr1->ops.ternary.opnd1, 0)
	      && operand_equal_p (expr0->ops.ternary.opnd1,
				  expr1->ops.ternary.opnd0, 0));

    case EXPR_CALL:
      {
        size_t i;

        /* If the calls are to different functions, then they
           clearly cannot be equal.  */
        if (!gimple_call_same_target_p (expr0->ops.call.fn_from,
                                        expr1->ops.call.fn_from))
          return false;

        if (! expr0->ops.call.pure)
          return false;

        if (expr0->ops.call.nargs !=  expr1->ops.call.nargs)
          return false;

        for (i = 0; i < expr0->ops.call.nargs; i++)
          if (! operand_equal_p (expr0->ops.call.args[i],
                                 expr1->ops.call.args[i], 0))
            return false;

        return true;
      }

    case EXPR_PHI:
      {
        size_t i;

        if (expr0->ops.phi.nargs !=  expr1->ops.phi.nargs)
          return false;

        for (i = 0; i < expr0->ops.phi.nargs; i++)
          if (! operand_equal_p (expr0->ops.phi.args[i],
                                 expr1->ops.phi.args[i], 0))
            return false;

        return true;
      }

    default:
      gcc_unreachable ();
    }
}

/* Generate a hash value for a pair of expressions.  This can be used
   iteratively by passing a previous result as the VAL argument.

   The same hash value is always returned for a given pair of expressions,
   regardless of the order in which they are presented.  This is useful in
   hashing the operands of commutative functions.  */

static hashval_t
iterative_hash_exprs_commutative (const_tree t1,
                                  const_tree t2, hashval_t val)
{
  hashval_t one = iterative_hash_expr (t1, 0);
  hashval_t two = iterative_hash_expr (t2, 0);
  hashval_t t;

  if (one > two)
    t = one, one = two, two = t;
  val = iterative_hash_hashval_t (one, val);
  val = iterative_hash_hashval_t (two, val);

  return val;
}

/* Compute a hash value for a hashable_expr value EXPR and a
   previously accumulated hash value VAL.  If two hashable_expr
   values compare equal with hashable_expr_equal_p, they must
   hash to the same value, given an identical value of VAL.
   The logic is intended to follow iterative_hash_expr in tree.c.  */

static hashval_t
iterative_hash_hashable_expr (const struct hashable_expr *expr, hashval_t val)
{
  switch (expr->kind)
    {
    case EXPR_SINGLE:
      val = iterative_hash_expr (expr->ops.single.rhs, val);
      break;

    case EXPR_UNARY:
      val = iterative_hash_object (expr->ops.unary.op, val);

      /* Make sure to include signedness in the hash computation.
         Don't hash the type, that can lead to having nodes which
         compare equal according to operand_equal_p, but which
         have different hash codes.  */
      if (CONVERT_EXPR_CODE_P (expr->ops.unary.op)
          || expr->ops.unary.op == NON_LVALUE_EXPR)
        val += TYPE_UNSIGNED (expr->type);

      val = iterative_hash_expr (expr->ops.unary.opnd, val);
      break;

    case EXPR_BINARY:
      val = iterative_hash_object (expr->ops.binary.op, val);
      if (commutative_tree_code (expr->ops.binary.op))
	val = iterative_hash_exprs_commutative (expr->ops.binary.opnd0,
						expr->ops.binary.opnd1, val);
      else
        {
          val = iterative_hash_expr (expr->ops.binary.opnd0, val);
          val = iterative_hash_expr (expr->ops.binary.opnd1, val);
        }
      break;

    case EXPR_TERNARY:
      val = iterative_hash_object (expr->ops.ternary.op, val);
      if (commutative_ternary_tree_code (expr->ops.ternary.op))
	val = iterative_hash_exprs_commutative (expr->ops.ternary.opnd0,
						expr->ops.ternary.opnd1, val);
      else
        {
          val = iterative_hash_expr (expr->ops.ternary.opnd0, val);
          val = iterative_hash_expr (expr->ops.ternary.opnd1, val);
        }
      val = iterative_hash_expr (expr->ops.ternary.opnd2, val);
      break;

    case EXPR_CALL:
      {
        size_t i;
        enum tree_code code = CALL_EXPR;
        gimple fn_from;

        val = iterative_hash_object (code, val);
        fn_from = expr->ops.call.fn_from;
        if (gimple_call_internal_p (fn_from))
          val = iterative_hash_hashval_t
            ((hashval_t) gimple_call_internal_fn (fn_from), val);
        else
          val = iterative_hash_expr (gimple_call_fn (fn_from), val);
        for (i = 0; i < expr->ops.call.nargs; i++)
          val = iterative_hash_expr (expr->ops.call.args[i], val);
      }
      break;

    case EXPR_PHI:
      {
        size_t i;

        for (i = 0; i < expr->ops.phi.nargs; i++)
          val = iterative_hash_expr (expr->ops.phi.args[i], val);
      }
      break;

    default:
      gcc_unreachable ();
    }

  return val;
}

/* Print a diagnostic dump of an expression hash table entry.  */

static void
print_expr_hash_elt (FILE * stream, const struct expr_hash_elt *element)
{
  if (element->stmt)
    fprintf (stream, "STMT ");
  else
    fprintf (stream, "COND ");

  if (element->lhs)
    {
      print_generic_expr (stream, element->lhs, 0);
      fprintf (stream, " = ");
    }

  switch (element->expr.kind)
    {
      case EXPR_SINGLE:
        print_generic_expr (stream, element->expr.ops.single.rhs, 0);
        break;

      case EXPR_UNARY:
	fprintf (stream, "%s ", get_tree_code_name (element->expr.ops.unary.op));
        print_generic_expr (stream, element->expr.ops.unary.opnd, 0);
        break;

      case EXPR_BINARY:
        print_generic_expr (stream, element->expr.ops.binary.opnd0, 0);
	fprintf (stream, " %s ", get_tree_code_name (element->expr.ops.binary.op));
        print_generic_expr (stream, element->expr.ops.binary.opnd1, 0);
        break;

      case EXPR_TERNARY:
	fprintf (stream, " %s <", get_tree_code_name (element->expr.ops.ternary.op));
        print_generic_expr (stream, element->expr.ops.ternary.opnd0, 0);
	fputs (", ", stream);
        print_generic_expr (stream, element->expr.ops.ternary.opnd1, 0);
	fputs (", ", stream);
        print_generic_expr (stream, element->expr.ops.ternary.opnd2, 0);
	fputs (">", stream);
        break;

      case EXPR_CALL:
        {
          size_t i;
          size_t nargs = element->expr.ops.call.nargs;
          gimple fn_from;

          fn_from = element->expr.ops.call.fn_from;
          if (gimple_call_internal_p (fn_from))
            fputs (internal_fn_name (gimple_call_internal_fn (fn_from)),
                   stream);
          else
            print_generic_expr (stream, gimple_call_fn (fn_from), 0);
          fprintf (stream, " (");
          for (i = 0; i < nargs; i++)
            {
              print_generic_expr (stream, element->expr.ops.call.args[i], 0);
              if (i + 1 < nargs)
                fprintf (stream, ", ");
            }
          fprintf (stream, ")");
        }
        break;

      case EXPR_PHI:
        {
          size_t i;
          size_t nargs = element->expr.ops.phi.nargs;

          fprintf (stream, "PHI <");
          for (i = 0; i < nargs; i++)
            {
              print_generic_expr (stream, element->expr.ops.phi.args[i], 0);
              if (i + 1 < nargs)
                fprintf (stream, ", ");
            }
          fprintf (stream, ">");
        }
        break;
    }
  fprintf (stream, "\n");

  if (element->stmt)
    {
      fprintf (stream, "          ");
      print_gimple_stmt (stream, element->stmt, 0, 0);
    }
}

/* Delete variable sized pieces of the expr_hash_elt ELEMENT.  */

static void
free_expr_hash_elt_contents (struct expr_hash_elt *element)
{
  if (element->expr.kind == EXPR_CALL)
    free (element->expr.ops.call.args);
  else if (element->expr.kind == EXPR_PHI)
    free (element->expr.ops.phi.args);
}

/* Delete an expr_hash_elt and reclaim its storage.  */

static void
free_expr_hash_elt (void *elt)
{
  struct expr_hash_elt *element = ((struct expr_hash_elt *)elt);
  free_expr_hash_elt_contents (element);
  free (element);
}

/* Allocate an EDGE_INFO for edge E and attach it to E.
   Return the new EDGE_INFO structure.  */

static struct edge_info *
allocate_edge_info (edge e)
{
  struct edge_info *edge_info;

  edge_info = XCNEW (struct edge_info);

  e->aux = edge_info;
  return edge_info;
}

/* Free all EDGE_INFO structures associated with edges in the CFG.
   If a particular edge can be threaded, copy the redirection
   target from the EDGE_INFO structure into the edge's AUX field
   as required by code to update the CFG and SSA graph for
   jump threading.  */

static void
free_all_edge_infos (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
	 struct edge_info *edge_info = (struct edge_info *) e->aux;

	  if (edge_info)
	    {
	      edge_info->cond_equivalences.release ();
	      free (edge_info);
	      e->aux = NULL;
	    }
	}
    }
}

class dom_opt_dom_walker : public dom_walker
{
public:
  dom_opt_dom_walker (cdi_direction direction)
    : dom_walker (direction), m_dummy_cond (NULL) {}

  virtual void before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

private:
  void thread_across_edge (edge);

  gimple m_dummy_cond;
};

/* Jump threading, redundancy elimination and const/copy propagation.

   This pass may expose new symbols that need to be renamed into SSA.  For
   every new symbol exposed, its corresponding bit will be set in
   VARS_TO_RENAME.  */

namespace {

const pass_data pass_data_dominator =
{
  GIMPLE_PASS, /* type */
  "dom", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TREE_SSA_DOMINATOR_OPTS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_dominator : public gimple_opt_pass
{
public:
  pass_dominator (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dominator, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_dominator (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dom != 0; }
  virtual unsigned int execute (function *);

}; // class pass_dominator

unsigned int
pass_dominator::execute (function *fun)
{
  memset (&opt_stats, 0, sizeof (opt_stats));

  /* Create our hash tables.  */
  avail_exprs.create (1024);
  avail_exprs_stack.create (20);
  const_and_copies_stack.create (20);
  need_eh_cleanup = BITMAP_ALLOC (NULL);

  calculate_dominance_info (CDI_DOMINATORS);
  cfg_altered = false;

  /* We need to know loop structures in order to avoid destroying them
     in jump threading.  Note that we still can e.g. thread through loop
     headers to an exit edge, or through loop header to the loop body, assuming
     that we update the loop info.

     TODO: We don't need to set LOOPS_HAVE_PREHEADERS generally, but due
     to several overly conservative bail-outs in jump threading, case
     gcc.dg/tree-ssa/pr21417.c can't be threaded if loop preheader is
     missing.  We should improve jump threading in future then
     LOOPS_HAVE_PREHEADERS won't be needed here.  */
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES);

  /* Initialize the value-handle array.  */
  threadedge_initialize_values ();

  /* We need accurate information regarding back edges in the CFG
     for jump threading; this may include back edges that are not part of
     a single loop.  */
  mark_dfs_back_edges ();

  /* Recursively walk the dominator tree optimizing statements.  */
  dom_opt_dom_walker (CDI_DOMINATORS).walk (fun->cfg->x_entry_block_ptr);

  {
    gimple_stmt_iterator gsi;
    basic_block bb;
    FOR_EACH_BB_FN (bb, fun)
      {
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  update_stmt_if_modified (gsi_stmt (gsi));
      }
  }

  /* If we exposed any new variables, go ahead and put them into
     SSA form now, before we handle jump threading.  This simplifies
     interactions between rewriting of _DECL nodes into SSA form
     and rewriting SSA_NAME nodes into SSA form after block
     duplication and CFG manipulation.  */
  update_ssa (TODO_update_ssa);

  free_all_edge_infos ();

  /* Thread jumps, creating duplicate blocks as needed.  */
  cfg_altered |= thread_through_all_blocks (first_pass_instance);

  if (cfg_altered)
    free_dominance_info (CDI_DOMINATORS);

  /* Removal of statements may make some EH edges dead.  Purge
     such edges from the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      unsigned i;
      bitmap_iterator bi;

      /* Jump threading may have created forwarder blocks from blocks
	 needing EH cleanup; the new successor of these blocks, which
	 has inherited from the original block, needs the cleanup.
	 Don't clear bits in the bitmap, as that can break the bitmap
	 iterator.  */
      EXECUTE_IF_SET_IN_BITMAP (need_eh_cleanup, 0, i, bi)
	{
	  basic_block bb = BASIC_BLOCK_FOR_FN (fun, i);
	  if (bb == NULL)
	    continue;
	  while (single_succ_p (bb)
		 && (single_succ_edge (bb)->flags & EDGE_EH) == 0)
	    bb = single_succ (bb);
	  if (bb == EXIT_BLOCK_PTR_FOR_FN (fun))
	    continue;
	  if ((unsigned) bb->index != i)
	    bitmap_set_bit (need_eh_cleanup, bb->index);
	}

      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      bitmap_clear (need_eh_cleanup);
    }

  statistics_counter_event (fun, "Redundant expressions eliminated",
			    opt_stats.num_re);
  statistics_counter_event (fun, "Constants propagated",
			    opt_stats.num_const_prop);
  statistics_counter_event (fun, "Copies propagated",
			    opt_stats.num_copy_prop);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    dump_dominator_optimization_stats (dump_file);

  loop_optimizer_finalize ();

  /* Delete our main hashtable.  */
  avail_exprs.dispose ();

  /* Free asserted bitmaps and stacks.  */
  BITMAP_FREE (need_eh_cleanup);

  avail_exprs_stack.release ();
  const_and_copies_stack.release ();

  /* Free the value-handle array.  */
  threadedge_finalize_values ();

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_dominator (gcc::context *ctxt)
{
  return new pass_dominator (ctxt);
}


/* Given a conditional statement CONDSTMT, convert the
   condition to a canonical form.  */

static void
canonicalize_comparison (gimple condstmt)
{
  tree op0;
  tree op1;
  enum tree_code code;

  gcc_assert (gimple_code (condstmt) == GIMPLE_COND);

  op0 = gimple_cond_lhs (condstmt);
  op1 = gimple_cond_rhs (condstmt);

  code = gimple_cond_code (condstmt);

  /* If it would be profitable to swap the operands, then do so to
     canonicalize the statement, enabling better optimization.

     By placing canonicalization of such expressions here we
     transparently keep statements in canonical form, even
     when the statement is modified.  */
  if (tree_swap_operands_p (op0, op1, false))
    {
      /* For relationals we need to swap the operands
	 and change the code.  */
      if (code == LT_EXPR
	  || code == GT_EXPR
	  || code == LE_EXPR
	  || code == GE_EXPR)
	{
          code = swap_tree_comparison (code);

          gimple_cond_set_code (condstmt, code);
          gimple_cond_set_lhs (condstmt, op1);
          gimple_cond_set_rhs (condstmt, op0);

          update_stmt (condstmt);
	}
    }
}

/* Initialize local stacks for this optimizer and record equivalences
   upon entry to BB.  Equivalences can come from the edge traversed to
   reach BB or they may come from PHI nodes at the start of BB.  */

/* Remove all the expressions in LOCALS from TABLE, stopping when there are
   LIMIT entries left in LOCALs.  */

static void
remove_local_expressions_from_table (void)
{
  /* Remove all the expressions made available in this block.  */
  while (avail_exprs_stack.length () > 0)
    {
      expr_hash_elt_t victim = avail_exprs_stack.pop ();
      expr_hash_elt **slot;

      if (victim == NULL)
	break;

      /* This must precede the actual removal from the hash table,
         as ELEMENT and the table entry may share a call argument
         vector which will be freed during removal.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "<<<< ");
          print_expr_hash_elt (dump_file, victim);
        }

      slot = avail_exprs.find_slot_with_hash (victim, victim->hash, NO_INSERT);
      gcc_assert (slot && *slot == victim);
      avail_exprs.clear_slot (slot);
    }
}

/* Use the source/dest pairs in CONST_AND_COPIES_STACK to restore
   CONST_AND_COPIES to its original state, stopping when we hit a
   NULL marker.  */

static void
restore_vars_to_original_value (void)
{
  while (const_and_copies_stack.length () > 0)
    {
      tree prev_value, dest;

      dest = const_and_copies_stack.pop ();

      if (dest == NULL)
	break;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "<<<< COPY ");
	  print_generic_expr (dump_file, dest, 0);
	  fprintf (dump_file, " = ");
	  print_generic_expr (dump_file, SSA_NAME_VALUE (dest), 0);
	  fprintf (dump_file, "\n");
	}

      prev_value = const_and_copies_stack.pop ();
      set_ssa_name_value (dest, prev_value);
    }
}

/* A trivial wrapper so that we can present the generic jump
   threading code with a simple API for simplifying statements.  */
static tree
simplify_stmt_for_jump_threading (gimple stmt,
				  gimple within_stmt ATTRIBUTE_UNUSED)
{
  return lookup_avail_expr (stmt, false);
}

/* Record into the equivalence tables any equivalences implied by
   traversing edge E (which are cached in E->aux).

   Callers are responsible for managing the unwinding markers.  */
static void
record_temporary_equivalences (edge e)
{
  int i;
  struct edge_info *edge_info = (struct edge_info *) e->aux;

  /* If we have info associated with this edge, record it into
     our equivalence tables.  */
  if (edge_info)
    {
      cond_equivalence *eq;
      tree lhs = edge_info->lhs;
      tree rhs = edge_info->rhs;

      /* If we have a simple NAME = VALUE equivalence, record it.  */
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	record_const_or_copy (lhs, rhs);

      /* If we have 0 = COND or 1 = COND equivalences, record them
	 into our expression hash tables.  */
      for (i = 0; edge_info->cond_equivalences.iterate (i, &eq); ++i)
	record_cond (eq);
    }
}

/* Wrapper for common code to attempt to thread an edge.  For example,
   it handles lazily building the dummy condition and the bookkeeping
   when jump threading is successful.  */

void
dom_opt_dom_walker::thread_across_edge (edge e)
{
  if (! m_dummy_cond)
    m_dummy_cond =
        gimple_build_cond (NE_EXPR,
                           integer_zero_node, integer_zero_node,
                           NULL, NULL);

  /* Push a marker on both stacks so we can unwind the tables back to their
     current state.  */
  avail_exprs_stack.safe_push (NULL);
  const_and_copies_stack.safe_push (NULL_TREE);

  /* Traversing E may result in equivalences we can utilize.  */
  record_temporary_equivalences (e);

  /* With all the edge equivalences in the tables, go ahead and attempt
     to thread through E->dest.  */
  ::thread_across_edge (m_dummy_cond, e, false,
		        &const_and_copies_stack,
		        simplify_stmt_for_jump_threading);

  /* And restore the various tables to their state before
     we threaded this edge. 

     XXX The code in tree-ssa-threadedge.c will restore the state of
     the const_and_copies table.  We we just have to restore the expression
     table.  */
  remove_local_expressions_from_table ();
}

/* PHI nodes can create equivalences too.

   Ignoring any alternatives which are the same as the result, if
   all the alternatives are equal, then the PHI node creates an
   equivalence.  */

static void
record_equivalences_from_phis (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);

      tree lhs = gimple_phi_result (phi);
      tree rhs = NULL;
      size_t i;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree t = gimple_phi_arg_def (phi, i);

	  /* Ignore alternatives which are the same as our LHS.  Since
	     LHS is a PHI_RESULT, it is known to be a SSA_NAME, so we
	     can simply compare pointers.  */
	  if (lhs == t)
	    continue;

	  /* If we have not processed an alternative yet, then set
	     RHS to this alternative.  */
	  if (rhs == NULL)
	    rhs = t;
	  /* If we have processed an alternative (stored in RHS), then
	     see if it is equal to this one.  If it isn't, then stop
	     the search.  */
	  else if (! operand_equal_for_phi_arg_p (rhs, t))
	    break;
	}

      /* If we had no interesting alternatives, then all the RHS alternatives
	 must have been the same as LHS.  */
      if (!rhs)
	rhs = lhs;

      /* If we managed to iterate through each PHI alternative without
	 breaking out of the loop, then we have a PHI which may create
	 a useful equivalence.  We do not need to record unwind data for
	 this, since this is a true assignment and not an equivalence
	 inferred from a comparison.  All uses of this ssa name are dominated
	 by this assignment, so unwinding just costs time and space.  */
      if (i == gimple_phi_num_args (phi) && may_propagate_copy (lhs, rhs))
	set_ssa_name_value (lhs, rhs);
    }
}

/* Ignoring loop backedges, if BB has precisely one incoming edge then
   return that edge.  Otherwise return NULL.  */
static edge
single_incoming_edge_ignoring_loop_edges (basic_block bb)
{
  edge retval = NULL;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* A loop back edge can be identified by the destination of
	 the edge dominating the source of the edge.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	continue;

      /* If we have already seen a non-loop edge, then we must have
	 multiple incoming non-loop edges and thus we return NULL.  */
      if (retval)
	return NULL;

      /* This is the first non-loop incoming edge we have found.  Record
	 it.  */
      retval = e;
    }

  return retval;
}

/* Record any equivalences created by the incoming edge to BB.  If BB
   has more than one incoming edge, then no equivalence is created.  */

static void
record_equivalences_from_incoming_edge (basic_block bb)
{
  edge e;
  basic_block parent;
  struct edge_info *edge_info;

  /* If our parent block ended with a control statement, then we may be
     able to record some equivalences based on which outgoing edge from
     the parent was followed.  */
  parent = get_immediate_dominator (CDI_DOMINATORS, bb);

  e = single_incoming_edge_ignoring_loop_edges (bb);

  /* If we had a single incoming edge from our parent block, then enter
     any data associated with the edge into our tables.  */
  if (e && e->src == parent)
    {
      unsigned int i;

      edge_info = (struct edge_info *) e->aux;

      if (edge_info)
	{
	  tree lhs = edge_info->lhs;
	  tree rhs = edge_info->rhs;
	  cond_equivalence *eq;

	  if (lhs)
	    record_equality (lhs, rhs);

	  /* If LHS is an SSA_NAME and RHS is a constant integer and LHS was
	     set via a widening type conversion, then we may be able to record
	     additional equivalences.  */
	  if (lhs
	      && TREE_CODE (lhs) == SSA_NAME
	      && is_gimple_constant (rhs)
	      && TREE_CODE (rhs) == INTEGER_CST)
	    {
	      gimple defstmt = SSA_NAME_DEF_STMT (lhs);

	      if (defstmt
		  && is_gimple_assign (defstmt)
		  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (defstmt)))
		{
		  tree old_rhs = gimple_assign_rhs1 (defstmt);

		  /* If the conversion widens the original value and
		     the constant is in the range of the type of OLD_RHS,
		     then convert the constant and record the equivalence.

		     Note that int_fits_type_p does not check the precision
		     if the upper and lower bounds are OK.  */
		  if (INTEGRAL_TYPE_P (TREE_TYPE (old_rhs))
		      && (TYPE_PRECISION (TREE_TYPE (lhs))
			  > TYPE_PRECISION (TREE_TYPE (old_rhs)))
		      && int_fits_type_p (rhs, TREE_TYPE (old_rhs)))
		    {
		      tree newval = fold_convert (TREE_TYPE (old_rhs), rhs);
		      record_equality (old_rhs, newval);
		    }
		}
	    }

	  for (i = 0; edge_info->cond_equivalences.iterate (i, &eq); ++i)
	    record_cond (eq);
	}
    }
}

/* Dump SSA statistics on FILE.  */

void
dump_dominator_optimization_stats (FILE *file)
{
  fprintf (file, "Total number of statements:                   %6ld\n\n",
	   opt_stats.num_stmts);
  fprintf (file, "Exprs considered for dominator optimizations: %6ld\n",
           opt_stats.num_exprs_considered);

  fprintf (file, "\nHash table statistics:\n");

  fprintf (file, "    avail_exprs: ");
  htab_statistics (file, avail_exprs);
}


/* Dump SSA statistics on stderr.  */

DEBUG_FUNCTION void
debug_dominator_optimization_stats (void)
{
  dump_dominator_optimization_stats (stderr);
}


/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, hash_table <expr_elt_hasher> htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab.size (),
	   (long) htab.elements (),
	   htab.collisions ());
}


/* Enter condition equivalence into the expression hash table.
   This indicates that a conditional expression has a known
   boolean value.  */

static void
record_cond (cond_equivalence *p)
{
  struct expr_hash_elt *element = XCNEW (struct expr_hash_elt);
  expr_hash_elt **slot;

  initialize_hash_element_from_expr (&p->cond, p->value, element);

  slot = avail_exprs.find_slot_with_hash (element, element->hash, INSERT);
  if (*slot == NULL)
    {
      *slot = element;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "1>>> ");
          print_expr_hash_elt (dump_file, element);
        }

      avail_exprs_stack.safe_push (element);
    }
  else
    free_expr_hash_elt (element);
}

/* Build a cond_equivalence record indicating that the comparison
   CODE holds between operands OP0 and OP1 and push it to **P.  */

static void
build_and_record_new_cond (enum tree_code code,
                           tree op0, tree op1,
                           vec<cond_equivalence> *p)
{
  cond_equivalence c;
  struct hashable_expr *cond = &c.cond;

  gcc_assert (TREE_CODE_CLASS (code) == tcc_comparison);

  cond->type = boolean_type_node;
  cond->kind = EXPR_BINARY;
  cond->ops.binary.op = code;
  cond->ops.binary.opnd0 = op0;
  cond->ops.binary.opnd1 = op1;

  c.value = boolean_true_node;
  p->safe_push (c);
}

/* Record that COND is true and INVERTED is false into the edge information
   structure.  Also record that any conditions dominated by COND are true
   as well.

   For example, if a < b is true, then a <= b must also be true.  */

static void
record_conditions (struct edge_info *edge_info, tree cond, tree inverted)
{
  tree op0, op1;
  cond_equivalence c;

  if (!COMPARISON_CLASS_P (cond))
    return;

  op0 = TREE_OPERAND (cond, 0);
  op1 = TREE_OPERAND (cond, 1);

  switch (TREE_CODE (cond))
    {
    case LT_EXPR:
    case GT_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	  build_and_record_new_cond (LTGT_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}

      build_and_record_new_cond ((TREE_CODE (cond) == LT_EXPR
				  ? LE_EXPR : GE_EXPR),
				 op0, op1, &edge_info->cond_equivalences);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case GE_EXPR:
    case LE_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}
      break;

    case EQ_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}
      build_and_record_new_cond (LE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (GE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNORDERED_EXPR:
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNEQ_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNLT_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGT_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNLT_EXPR:
    case UNGT_EXPR:
      build_and_record_new_cond ((TREE_CODE (cond) == UNLT_EXPR
				  ? UNLE_EXPR : UNGE_EXPR),
				 op0, op1, &edge_info->cond_equivalences);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNEQ_EXPR:
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case LTGT_EXPR:
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    default:
      break;
    }

  /* Now store the original true and false conditions into the first
     two slots.  */
  initialize_expr_from_cond (cond, &c.cond);
  c.value = boolean_true_node;
  edge_info->cond_equivalences.safe_push (c);

  /* It is possible for INVERTED to be the negation of a comparison,
     and not a valid RHS or GIMPLE_COND condition.  This happens because
     invert_truthvalue may return such an expression when asked to invert
     a floating-point comparison.  These comparisons are not assumed to
     obey the trichotomy law.  */
  initialize_expr_from_cond (inverted, &c.cond);
  c.value = boolean_false_node;
  edge_info->cond_equivalences.safe_push (c);
}

/* A helper function for record_const_or_copy and record_equality.
   Do the work of recording the value and undo info.  */

static void
record_const_or_copy_1 (tree x, tree y, tree prev_x)
{
  set_ssa_name_value (x, y);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "0>>> COPY ");
      print_generic_expr (dump_file, x, 0);
      fprintf (dump_file, " = ");
      print_generic_expr (dump_file, y, 0);
      fprintf (dump_file, "\n");
    }

  const_and_copies_stack.reserve (2);
  const_and_copies_stack.quick_push (prev_x);
  const_and_copies_stack.quick_push (x);
}

/* Return the loop depth of the basic block of the defining statement of X.
   This number should not be treated as absolutely correct because the loop
   information may not be completely up-to-date when dom runs.  However, it
   will be relatively correct, and as more passes are taught to keep loop info
   up to date, the result will become more and more accurate.  */

int
loop_depth_of_name (tree x)
{
  gimple defstmt;
  basic_block defbb;

  /* If it's not an SSA_NAME, we have no clue where the definition is.  */
  if (TREE_CODE (x) != SSA_NAME)
    return 0;

  /* Otherwise return the loop depth of the defining statement's bb.
     Note that there may not actually be a bb for this statement, if the
     ssa_name is live on entry.  */
  defstmt = SSA_NAME_DEF_STMT (x);
  defbb = gimple_bb (defstmt);
  if (!defbb)
    return 0;

  return bb_loop_depth (defbb);
}

/* Record that X is equal to Y in const_and_copies.  Record undo
   information in the block-local vector.  */

static void
record_const_or_copy (tree x, tree y)
{
  tree prev_x = SSA_NAME_VALUE (x);

  gcc_assert (TREE_CODE (x) == SSA_NAME);

  if (TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      if (tmp)
	y = tmp;
    }

  record_const_or_copy_1 (x, y, prev_x);
}

/* Similarly, but assume that X and Y are the two operands of an EQ_EXPR.
   This constrains the cases in which we may treat this as assignment.  */

static void
record_equality (tree x, tree y)
{
  tree prev_x = NULL, prev_y = NULL;

  if (TREE_CODE (x) == SSA_NAME)
    prev_x = SSA_NAME_VALUE (x);
  if (TREE_CODE (y) == SSA_NAME)
    prev_y = SSA_NAME_VALUE (y);

  /* If one of the previous values is invariant, or invariant in more loops
     (by depth), then use that.
     Otherwise it doesn't matter which value we choose, just so
     long as we canonicalize on one value.  */
  if (is_gimple_min_invariant (y))
    ;
  else if (is_gimple_min_invariant (x)
	   || (loop_depth_of_name (x) <= loop_depth_of_name (y)))
    prev_x = x, x = y, y = prev_x, prev_x = prev_y;
  else if (prev_x && is_gimple_min_invariant (prev_x))
    x = y, y = prev_x, prev_x = prev_y;
  else if (prev_y)
    y = prev_y;

  /* After the swapping, we must have one SSA_NAME.  */
  if (TREE_CODE (x) != SSA_NAME)
    return;

  /* For IEEE, -0.0 == 0.0, so we don't necessarily know the sign of a
     variable compared against zero.  If we're honoring signed zeros,
     then we cannot record this value unless we know that the value is
     nonzero.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (x)))
      && (TREE_CODE (y) != REAL_CST
	  || REAL_VALUES_EQUAL (dconst0, TREE_REAL_CST (y))))
    return;

  record_const_or_copy_1 (x, y, prev_x);
}

/* Returns true when STMT is a simple iv increment.  It detects the
   following situation:

   i_1 = phi (..., i_2)
   i_2 = i_1 +/- ...  */

bool
simple_iv_increment_p (gimple stmt)
{
  enum tree_code code;
  tree lhs, preinc;
  gimple phi;
  size_t i;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (stmt);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (code != PLUS_EXPR
      && code != MINUS_EXPR
      && code != POINTER_PLUS_EXPR)
    return false;

  preinc = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (preinc) != SSA_NAME)
    return false;

  phi = SSA_NAME_DEF_STMT (preinc);
  if (gimple_code (phi) != GIMPLE_PHI)
    return false;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_def (phi, i) == lhs)
      return true;

  return false;
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).

   Propagate values from CONST_AND_COPIES into the PHI nodes of the
   successors of BB.  */

static void
cprop_into_successor_phis (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      int indx;
      gimple_stmt_iterator gsi;

      /* If this is an abnormal edge, then we do not want to copy propagate
	 into the PHI alternative associated with this edge.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      gsi = gsi_start_phis (e->dest);
      if (gsi_end_p (gsi))
	continue;

      /* We may have an equivalence associated with this edge.  While
	 we can not propagate it into non-dominated blocks, we can
	 propagate them into PHIs in non-dominated blocks.  */

      /* Push the unwind marker so we can reset the const and copies
	 table back to its original state after processing this edge.  */
      const_and_copies_stack.safe_push (NULL_TREE);

      /* Extract and record any simple NAME = VALUE equivalences.

	 Don't bother with [01] = COND equivalences, they're not useful
	 here.  */
      struct edge_info *edge_info = (struct edge_info *) e->aux;
      if (edge_info)
	{
	  tree lhs = edge_info->lhs;
	  tree rhs = edge_info->rhs;

	  if (lhs && TREE_CODE (lhs) == SSA_NAME)
	    record_const_or_copy (lhs, rhs);
	}

      indx = e->dest_idx;
      for ( ; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  tree new_val;
	  use_operand_p orig_p;
	  tree orig_val;
          gimple phi = gsi_stmt (gsi);

	  /* The alternative may be associated with a constant, so verify
	     it is an SSA_NAME before doing anything with it.  */
	  orig_p = gimple_phi_arg_imm_use_ptr (phi, indx);
	  orig_val = get_use_from_ptr (orig_p);
	  if (TREE_CODE (orig_val) != SSA_NAME)
	    continue;

	  /* If we have *ORIG_P in our constant/copy table, then replace
	     ORIG_P with its value in our constant/copy table.  */
	  new_val = SSA_NAME_VALUE (orig_val);
	  if (new_val
	      && new_val != orig_val
	      && (TREE_CODE (new_val) == SSA_NAME
		  || is_gimple_min_invariant (new_val))
	      && may_propagate_copy (orig_val, new_val))
	    propagate_value (orig_p, new_val);
	}

      restore_vars_to_original_value ();
    }
}

/* We have finished optimizing BB, record any information implied by
   taking a specific outgoing edge from BB.  */

static void
record_edge_info (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  struct edge_info *edge_info;

  if (! gsi_end_p (gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      location_t loc = gimple_location (stmt);

      if (gimple_code (stmt) == GIMPLE_SWITCH)
	{
	  tree index = gimple_switch_index (stmt);

	  if (TREE_CODE (index) == SSA_NAME)
	    {
	      int i;
              int n_labels = gimple_switch_num_labels (stmt);
	      tree *info = XCNEWVEC (tree, last_basic_block_for_fn (cfun));
	      edge e;
	      edge_iterator ei;

	      for (i = 0; i < n_labels; i++)
		{
		  tree label = gimple_switch_label (stmt, i);
		  basic_block target_bb = label_to_block (CASE_LABEL (label));
		  if (CASE_HIGH (label)
		      || !CASE_LOW (label)
		      || info[target_bb->index])
		    info[target_bb->index] = error_mark_node;
		  else
		    info[target_bb->index] = label;
		}

	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  basic_block target_bb = e->dest;
		  tree label = info[target_bb->index];

		  if (label != NULL && label != error_mark_node)
		    {
		      tree x = fold_convert_loc (loc, TREE_TYPE (index),
						 CASE_LOW (label));
		      edge_info = allocate_edge_info (e);
		      edge_info->lhs = index;
		      edge_info->rhs = x;
		    }
		}
	      free (info);
	    }
	}

      /* A COND_EXPR may create equivalences too.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  edge true_edge;
	  edge false_edge;

          tree op0 = gimple_cond_lhs (stmt);
          tree op1 = gimple_cond_rhs (stmt);
          enum tree_code code = gimple_cond_code (stmt);

	  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

          /* Special case comparing booleans against a constant as we
             know the value of OP0 on both arms of the branch.  i.e., we
             can record an equivalence for OP0 rather than COND.  */
          if ((code == EQ_EXPR || code == NE_EXPR)
              && TREE_CODE (op0) == SSA_NAME
              && TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE
              && is_gimple_min_invariant (op1))
            {
              if (code == EQ_EXPR)
                {
                  edge_info = allocate_edge_info (true_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_false_node
                                    : boolean_true_node);

                  edge_info = allocate_edge_info (false_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_true_node
                                    : boolean_false_node);
                }
              else
                {
                  edge_info = allocate_edge_info (true_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_true_node
                                    : boolean_false_node);

                  edge_info = allocate_edge_info (false_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_false_node
                                    : boolean_true_node);
                }
            }
          else if (is_gimple_min_invariant (op0)
                   && (TREE_CODE (op1) == SSA_NAME
                       || is_gimple_min_invariant (op1)))
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
              bool can_infer_simple_equiv
                = !(HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (op0)))
                    && real_zerop (op0));
              struct edge_info *edge_info;

              edge_info = allocate_edge_info (true_edge);
              record_conditions (edge_info, cond, inverted);

              if (can_infer_simple_equiv && code == EQ_EXPR)
                {
                  edge_info->lhs = op1;
                  edge_info->rhs = op0;
                }

              edge_info = allocate_edge_info (false_edge);
              record_conditions (edge_info, inverted, cond);

              if (can_infer_simple_equiv && TREE_CODE (inverted) == EQ_EXPR)
                {
                  edge_info->lhs = op1;
                  edge_info->rhs = op0;
                }
            }

          else if (TREE_CODE (op0) == SSA_NAME
                   && (TREE_CODE (op1) == SSA_NAME
                       || is_gimple_min_invariant (op1)))
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
              bool can_infer_simple_equiv
                = !(HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (op1)))
                    && (TREE_CODE (op1) == SSA_NAME || real_zerop (op1)));
              struct edge_info *edge_info;

              edge_info = allocate_edge_info (true_edge);
              record_conditions (edge_info, cond, inverted);

              if (can_infer_simple_equiv && code == EQ_EXPR)
                {
                  edge_info->lhs = op0;
                  edge_info->rhs = op1;
                }

              edge_info = allocate_edge_info (false_edge);
              record_conditions (edge_info, inverted, cond);

              if (can_infer_simple_equiv && TREE_CODE (inverted) == EQ_EXPR)
                {
                  edge_info->lhs = op0;
                  edge_info->rhs = op1;
                }
            }
        }

      /* ??? TRUTH_NOT_EXPR can create an equivalence too.  */
    }
}

void
dom_opt_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nOptimizing block #%d\n\n", bb->index);

  /* Push a marker on the stacks of local information so that we know how
     far to unwind when we finalize this block.  */
  avail_exprs_stack.safe_push (NULL);
  const_and_copies_stack.safe_push (NULL_TREE);

  record_equivalences_from_incoming_edge (bb);

  /* PHI nodes can create equivalences too.  */
  record_equivalences_from_phis (bb);

  /* Create equivalences from redundant PHIs.  PHIs are only truly
     redundant when they exist in the same block, so push another
     marker and unwind right afterwards.  */
  avail_exprs_stack.safe_push (NULL);
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    eliminate_redundant_computations (&gsi);
  remove_local_expressions_from_table ();

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    optimize_stmt (bb, gsi);

  /* Now prepare to process dominated blocks.  */
  record_edge_info (bb);
  cprop_into_successor_phis (bb);
}

/* We have finished processing the dominator children of BB, perform
   any finalization actions in preparation for leaving this node in
   the dominator tree.  */

void
dom_opt_dom_walker::after_dom_children (basic_block bb)
{
  gimple last;

  /* If we have an outgoing edge to a block with multiple incoming and
     outgoing edges, then we may be able to thread the edge, i.e., we
     may be able to statically determine which of the outgoing edges
     will be traversed when the incoming edge from BB is traversed.  */
  if (single_succ_p (bb)
      && (single_succ_edge (bb)->flags & EDGE_ABNORMAL) == 0
      && potentially_threadable_block (single_succ (bb)))
    {
      thread_across_edge (single_succ_edge (bb));
    }
  else if ((last = last_stmt (bb))
	   && gimple_code (last) == GIMPLE_COND
	   && EDGE_COUNT (bb->succs) == 2
	   && (EDGE_SUCC (bb, 0)->flags & EDGE_ABNORMAL) == 0
	   && (EDGE_SUCC (bb, 1)->flags & EDGE_ABNORMAL) == 0)
    {
      edge true_edge, false_edge;

      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      /* Only try to thread the edge if it reaches a target block with
	 more than one predecessor and more than one successor.  */
      if (potentially_threadable_block (true_edge->dest))
	thread_across_edge (true_edge);

      /* Similarly for the ELSE arm.  */
      if (potentially_threadable_block (false_edge->dest))
	thread_across_edge (false_edge);

    }

  /* These remove expressions local to BB from the tables.  */
  remove_local_expressions_from_table ();
  restore_vars_to_original_value ();
}

/* Search for redundant computations in STMT.  If any are found, then
   replace them with the variable holding the result of the computation.

   If safe, record this expression into the available expression hash
   table.  */

static void
eliminate_redundant_computations (gimple_stmt_iterator* gsi)
{
  tree expr_type;
  tree cached_lhs;
  tree def;
  bool insert = true;
  bool assigns_var_p = false;

  gimple stmt = gsi_stmt (*gsi);

  if (gimple_code (stmt) == GIMPLE_PHI)
    def = gimple_phi_result (stmt);
  else
    def = gimple_get_lhs (stmt);

  /* Certain expressions on the RHS can be optimized away, but can not
     themselves be entered into the hash tables.  */
  if (! def
      || TREE_CODE (def) != SSA_NAME
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def)
      || gimple_vdef (stmt)
      /* Do not record equivalences for increments of ivs.  This would create
	 overlapping live ranges for a very questionable gain.  */
      || simple_iv_increment_p (stmt))
    insert = false;

  /* Check if the expression has been computed before.  */
  cached_lhs = lookup_avail_expr (stmt, insert);

  opt_stats.num_exprs_considered++;

  /* Get the type of the expression we are trying to optimize.  */
  if (is_gimple_assign (stmt))
    {
      expr_type = TREE_TYPE (gimple_assign_lhs (stmt));
      assigns_var_p = true;
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    expr_type = boolean_type_node;
  else if (is_gimple_call (stmt))
    {
      gcc_assert (gimple_call_lhs (stmt));
      expr_type = TREE_TYPE (gimple_call_lhs (stmt));
      assigns_var_p = true;
    }
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    expr_type = TREE_TYPE (gimple_switch_index (stmt));
  else if (gimple_code (stmt) == GIMPLE_PHI)
    /* We can't propagate into a phi, so the logic below doesn't apply.
       Instead record an equivalence between the cached LHS and the
       PHI result of this statement, provided they are in the same block.
       This should be sufficient to kill the redundant phi.  */
    {
      if (def && cached_lhs)
	record_const_or_copy (def, cached_lhs);
      return;
    }
  else
    gcc_unreachable ();

  if (!cached_lhs)
    return;

  /* It is safe to ignore types here since we have already done
     type checking in the hashing and equality routines.  In fact
     type checking here merely gets in the way of constant
     propagation.  Also, make sure that it is safe to propagate
     CACHED_LHS into the expression in STMT.  */
  if ((TREE_CODE (cached_lhs) != SSA_NAME
       && (assigns_var_p
           || useless_type_conversion_p (expr_type, TREE_TYPE (cached_lhs))))
      || may_propagate_copy_into_stmt (stmt, cached_lhs))
  {
      gcc_checking_assert (TREE_CODE (cached_lhs) == SSA_NAME
			   || is_gimple_min_invariant (cached_lhs));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced redundant expr '");
	  print_gimple_expr (dump_file, stmt, 0, dump_flags);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, cached_lhs, dump_flags);
          fprintf (dump_file, "'\n");
	}

      opt_stats.num_re++;

      if (assigns_var_p
	  && !useless_type_conversion_p (expr_type, TREE_TYPE (cached_lhs)))
	cached_lhs = fold_convert (expr_type, cached_lhs);

      propagate_tree_value_into_stmt (gsi, cached_lhs);

      /* Since it is always necessary to mark the result as modified,
         perhaps we should move this into propagate_tree_value_into_stmt
         itself.  */
      gimple_set_modified (gsi_stmt (*gsi), true);
  }
}

/* STMT, a GIMPLE_ASSIGN, may create certain equivalences, in either
   the available expressions table or the const_and_copies table.
   Detect and record those equivalences.  */
/* We handle only very simple copy equivalences here.  The heavy
   lifing is done by eliminate_redundant_computations.  */

static void
record_equivalences_from_stmt (gimple stmt, int may_optimize_p)
{
  tree lhs;
  enum tree_code lhs_code;

  gcc_assert (is_gimple_assign (stmt));

  lhs = gimple_assign_lhs (stmt);
  lhs_code = TREE_CODE (lhs);

  if (lhs_code == SSA_NAME
      && gimple_assign_single_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);

      /* If the RHS of the assignment is a constant or another variable that
	 may be propagated, register it in the CONST_AND_COPIES table.  We
	 do not need to record unwind data for this, since this is a true
	 assignment and not an equivalence inferred from a comparison.  All
	 uses of this ssa name are dominated by this assignment, so unwinding
	 just costs time and space.  */
      if (may_optimize_p
	  && (TREE_CODE (rhs) == SSA_NAME
	      || is_gimple_min_invariant (rhs)))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "==== ASGN ");
	    print_generic_expr (dump_file, lhs, 0);
	    fprintf (dump_file, " = ");
	    print_generic_expr (dump_file, rhs, 0);
	    fprintf (dump_file, "\n");
	  }

	set_ssa_name_value (lhs, rhs);
      }
    }

  /* A memory store, even an aliased store, creates a useful
     equivalence.  By exchanging the LHS and RHS, creating suitable
     vops and recording the result in the available expression table,
     we may be able to expose more redundant loads.  */
  if (!gimple_has_volatile_ops (stmt)
      && gimple_references_memory_p (stmt)
      && gimple_assign_single_p (stmt)
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
      && !is_gimple_reg (lhs))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      gimple new_stmt;

      /* Build a new statement with the RHS and LHS exchanged.  */
      if (TREE_CODE (rhs) == SSA_NAME)
        {
          /* NOTE tuples.  The call to gimple_build_assign below replaced
             a call to build_gimple_modify_stmt, which did not set the
             SSA_NAME_DEF_STMT on the LHS of the assignment.  Doing so
             may cause an SSA validation failure, as the LHS may be a
             default-initialized name and should have no definition.  I'm
             a bit dubious of this, as the artificial statement that we
             generate here may in fact be ill-formed, but it is simply
             used as an internal device in this pass, and never becomes
             part of the CFG.  */
          gimple defstmt = SSA_NAME_DEF_STMT (rhs);
          new_stmt = gimple_build_assign (rhs, lhs);
          SSA_NAME_DEF_STMT (rhs) = defstmt;
        }
      else
        new_stmt = gimple_build_assign (rhs, lhs);

      gimple_set_vuse (new_stmt, gimple_vdef (stmt));

      /* Finally enter the statement into the available expression
	 table.  */
      lookup_avail_expr (new_stmt, true);
    }
}

/* Replace *OP_P in STMT with any known equivalent value for *OP_P from
   CONST_AND_COPIES.  */

static void
cprop_operand (gimple stmt, use_operand_p op_p)
{
  tree val;
  tree op = USE_FROM_PTR (op_p);

  /* If the operand has a known constant value or it is known to be a
     copy of some other variable, use the value or copy stored in
     CONST_AND_COPIES.  */
  val = SSA_NAME_VALUE (op);
  if (val && val != op)
    {
      /* Do not replace hard register operands in asm statements.  */
      if (gimple_code (stmt) == GIMPLE_ASM
	  && !may_propagate_copy_into_asm (op))
	return;

      /* Certain operands are not allowed to be copy propagated due
	 to their interaction with exception handling and some GCC
	 extensions.  */
      if (!may_propagate_copy (op, val))
	return;

      /* Do not propagate addresses that point to volatiles into memory
	 stmts without volatile operands.  */
      if (POINTER_TYPE_P (TREE_TYPE (val))
	  && TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (val)))
	  && gimple_has_mem_ops (stmt)
	  && !gimple_has_volatile_ops (stmt))
	return;

      /* Do not propagate copies if the propagated value is at a deeper loop
	 depth than the propagatee.  Otherwise, this may move loop variant
	 variables outside of their loops and prevent coalescing
	 opportunities.  If the value was loop invariant, it will be hoisted
	 by LICM and exposed for copy propagation.  */
      if (loop_depth_of_name (val) > loop_depth_of_name (op))
	return;

      /* Do not propagate copies into simple IV increment statements.
         See PR23821 for how this can disturb IV analysis.  */
      if (TREE_CODE (val) != INTEGER_CST
	  && simple_iv_increment_p (stmt))
	return;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, op, dump_flags);
	  fprintf (dump_file, "' with %s '",
		   (TREE_CODE (val) != SSA_NAME ? "constant" : "variable"));
	  print_generic_expr (dump_file, val, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      if (TREE_CODE (val) != SSA_NAME)
	opt_stats.num_const_prop++;
      else
	opt_stats.num_copy_prop++;

      propagate_value (op_p, val);

      /* And note that we modified this statement.  This is now
	 safe, even if we changed virtual operands since we will
	 rescan the statement and rewrite its operands again.  */
      gimple_set_modified (stmt, true);
    }
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).

   Propagate values from CONST_AND_COPIES into the uses, vuses and
   vdef_ops of STMT.  */

static void
cprop_into_stmt (gimple stmt)
{
  use_operand_p op_p;
  ssa_op_iter iter;

  FOR_EACH_SSA_USE_OPERAND (op_p, stmt, iter, SSA_OP_USE)
    cprop_operand (stmt, op_p);
}

/* Optimize the statement pointed to by iterator SI.

   We try to perform some simplistic global redundancy elimination and
   constant propagation:

   1- To detect global redundancy, we keep track of expressions that have
      been computed in this block and its dominators.  If we find that the
      same expression is computed more than once, we eliminate repeated
      computations by using the target of the first one.

   2- Constant values and copy assignments.  This is used to do very
      simplistic constant and copy propagation.  When a constant or copy
      assignment is found, we map the value on the RHS of the assignment to
      the variable in the LHS in the CONST_AND_COPIES table.  */

static void
optimize_stmt (basic_block bb, gimple_stmt_iterator si)
{
  gimple stmt, old_stmt;
  bool may_optimize_p;
  bool modified_p = false;

  old_stmt = stmt = gsi_stmt (si);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Optimizing statement ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (gimple_code (stmt) == GIMPLE_COND)
    canonicalize_comparison (stmt);

  update_stmt_if_modified (stmt);
  opt_stats.num_stmts++;

  /* Const/copy propagate into USES, VUSES and the RHS of VDEFs.  */
  cprop_into_stmt (stmt);

  /* If the statement has been modified with constant replacements,
     fold its RHS before checking for redundant computations.  */
  if (gimple_modified_p (stmt))
    {
      tree rhs = NULL;

      /* Try to fold the statement making sure that STMT is kept
	 up to date.  */
      if (fold_stmt (&si))
	{
	  stmt = gsi_stmt (si);
	  gimple_set_modified (stmt, true);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Folded to: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	}

      /* We only need to consider cases that can yield a gimple operand.  */
      if (gimple_assign_single_p (stmt))
        rhs = gimple_assign_rhs1 (stmt);
      else if (gimple_code (stmt) == GIMPLE_GOTO)
        rhs = gimple_goto_dest (stmt);
      else if (gimple_code (stmt) == GIMPLE_SWITCH)
        /* This should never be an ADDR_EXPR.  */
        rhs = gimple_switch_index (stmt);

      if (rhs && TREE_CODE (rhs) == ADDR_EXPR)
        recompute_tree_invariant_for_addr_expr (rhs);

      /* Indicate that maybe_clean_or_replace_eh_stmt needs to be called,
	 even if fold_stmt updated the stmt already and thus cleared
	 gimple_modified_p flag on it.  */
      modified_p = true;
    }

  /* Check for redundant computations.  Do this optimization only
     for assignments that have no volatile ops and conditionals.  */
  may_optimize_p = (!gimple_has_side_effects (stmt)
                    && (is_gimple_assign (stmt)
                        || (is_gimple_call (stmt)
                            && gimple_call_lhs (stmt) != NULL_TREE)
                        || gimple_code (stmt) == GIMPLE_COND
                        || gimple_code (stmt) == GIMPLE_SWITCH));

  if (may_optimize_p)
    {
      if (gimple_code (stmt) == GIMPLE_CALL)
	{
	  /* Resolve __builtin_constant_p.  If it hasn't been
	     folded to integer_one_node by now, it's fairly
	     certain that the value simply isn't constant.  */
	  tree callee = gimple_call_fndecl (stmt);
	  if (callee
	      && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (callee) == BUILT_IN_CONSTANT_P)
	    {
	      propagate_tree_value_into_stmt (&si, integer_zero_node);
	      stmt = gsi_stmt (si);
	    }
	}

      update_stmt_if_modified (stmt);
      eliminate_redundant_computations (&si);
      stmt = gsi_stmt (si);

      /* Perform simple redundant store elimination.  */
      if (gimple_assign_single_p (stmt)
	  && TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs = gimple_assign_rhs1 (stmt);
	  tree cached_lhs;
	  gimple new_stmt;
	  if (TREE_CODE (rhs) == SSA_NAME)
	    {
	      tree tem = SSA_NAME_VALUE (rhs);
	      if (tem)
		rhs = tem;
	    }
	  /* Build a new statement with the RHS and LHS exchanged.  */
	  if (TREE_CODE (rhs) == SSA_NAME)
	    {
	      gimple defstmt = SSA_NAME_DEF_STMT (rhs);
	      new_stmt = gimple_build_assign (rhs, lhs);
	      SSA_NAME_DEF_STMT (rhs) = defstmt;
	    }
	  else
	    new_stmt = gimple_build_assign (rhs, lhs);
	  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	  cached_lhs = lookup_avail_expr (new_stmt, false);
	  if (cached_lhs
	      && rhs == cached_lhs)
	    {
	      basic_block bb = gimple_bb (stmt);
	      unlink_stmt_vdef (stmt);
	      if (gsi_remove (&si, true))
		{
		  bitmap_set_bit (need_eh_cleanup, bb->index);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  Flagged to clear EH edges.\n");
		}
	      release_defs (stmt);
	      return;
	    }
	}
    }

  /* Record any additional equivalences created by this statement.  */
  if (is_gimple_assign (stmt))
    record_equivalences_from_stmt (stmt, may_optimize_p);

  /* If STMT is a COND_EXPR and it was modified, then we may know
     where it goes.  If that is the case, then mark the CFG as altered.

     This will cause us to later call remove_unreachable_blocks and
     cleanup_tree_cfg when it is safe to do so.  It is not safe to
     clean things up here since removal of edges and such can trigger
     the removal of PHI nodes, which in turn can release SSA_NAMEs to
     the manager.

     That's all fine and good, except that once SSA_NAMEs are released
     to the manager, we must not call create_ssa_name until all references
     to released SSA_NAMEs have been eliminated.

     All references to the deleted SSA_NAMEs can not be eliminated until
     we remove unreachable blocks.

     We can not remove unreachable blocks until after we have completed
     any queued jump threading.

     We can not complete any queued jump threads until we have taken
     appropriate variables out of SSA form.  Taking variables out of
     SSA form can call create_ssa_name and thus we lose.

     Ultimately I suspect we're going to need to change the interface
     into the SSA_NAME manager.  */
  if (gimple_modified_p (stmt) || modified_p)
    {
      tree val = NULL;

      update_stmt_if_modified (stmt);

      if (gimple_code (stmt) == GIMPLE_COND)
        val = fold_binary_loc (gimple_location (stmt),
			   gimple_cond_code (stmt), boolean_type_node,
                           gimple_cond_lhs (stmt),  gimple_cond_rhs (stmt));
      else if (gimple_code (stmt) == GIMPLE_SWITCH)
	val = gimple_switch_index (stmt);

      if (val && TREE_CODE (val) == INTEGER_CST && find_taken_edge (bb, val))
	cfg_altered = true;

      /* If we simplified a statement in such a way as to be shown that it
	 cannot trap, update the eh information and the cfg to match.  */
      if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	{
	  bitmap_set_bit (need_eh_cleanup, bb->index);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Flagged to clear EH edges.\n");
	}
    }
}

/* Search for an existing instance of STMT in the AVAIL_EXPRS table.
   If found, return its LHS. Otherwise insert STMT in the table and
   return NULL_TREE.

   Also, when an expression is first inserted in the  table, it is also
   is also added to AVAIL_EXPRS_STACK, so that it can be removed when
   we finish processing this block and its children.  */

static tree
lookup_avail_expr (gimple stmt, bool insert)
{
  expr_hash_elt **slot;
  tree lhs;
  tree temp;
  struct expr_hash_elt element;

  /* Get LHS of phi, assignment, or call; else NULL_TREE.  */
  if (gimple_code (stmt) == GIMPLE_PHI)
    lhs = gimple_phi_result (stmt);
  else
    lhs = gimple_get_lhs (stmt);

  initialize_hash_element (stmt, lhs, &element);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "LKUP ");
      print_expr_hash_elt (dump_file, &element);
    }

  /* Don't bother remembering constant assignments and copy operations.
     Constants and copy operations are handled by the constant/copy propagator
     in optimize_stmt.  */
  if (element.expr.kind == EXPR_SINGLE
      && (TREE_CODE (element.expr.ops.single.rhs) == SSA_NAME
          || is_gimple_min_invariant (element.expr.ops.single.rhs)))
    return NULL_TREE;

  /* Finally try to find the expression in the main expression hash table.  */
  slot = avail_exprs.find_slot_with_hash (&element, element.hash,
					  (insert ? INSERT : NO_INSERT));
  if (slot == NULL)
    {
      free_expr_hash_elt_contents (&element);
      return NULL_TREE;
    }
  else if (*slot == NULL)
    {
      struct expr_hash_elt *element2 = XNEW (struct expr_hash_elt);
      *element2 = element;
      element2->stamp = element2;
      *slot = element2;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "2>>> ");
          print_expr_hash_elt (dump_file, element2);
        }

      avail_exprs_stack.safe_push (element2);
      return NULL_TREE;
    }
  else
    free_expr_hash_elt_contents (&element);

  /* Extract the LHS of the assignment so that it can be used as the current
     definition of another variable.  */
  lhs = ((struct expr_hash_elt *)*slot)->lhs;

  /* See if the LHS appears in the CONST_AND_COPIES table.  If it does, then
     use the value from the const_and_copies table.  */
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      temp = SSA_NAME_VALUE (lhs);
      if (temp)
	lhs = temp;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "FIND: ");
      print_generic_expr (dump_file, lhs, 0);
      fprintf (dump_file, "\n");
    }

  return lhs;
}

/* Hashing and equality functions for AVAIL_EXPRS.  We compute a value number
   for expressions using the code of the expression and the SSA numbers of
   its operands.  */

static hashval_t
avail_expr_hash (const void *p)
{
  gimple stmt = ((const struct expr_hash_elt *)p)->stmt;
  const struct hashable_expr *expr = &((const struct expr_hash_elt *)p)->expr;
  tree vuse;
  hashval_t val = 0;

  val = iterative_hash_hashable_expr (expr, val);

  /* If the hash table entry is not associated with a statement, then we
     can just hash the expression and not worry about virtual operands
     and such.  */
  if (!stmt)
    return val;

  /* Add the SSA version numbers of the vuse operand.  This is important
     because compound variables like arrays are not renamed in the
     operands.  Rather, the rename is done on the virtual variable
     representing all the elements of the array.  */
  if ((vuse = gimple_vuse (stmt)))
    val = iterative_hash_expr (vuse, val);

  return val;
}

/* PHI-ONLY copy and constant propagation.  This pass is meant to clean
   up degenerate PHIs created by or exposed by jump threading.  */

/* Given a statement STMT, which is either a PHI node or an assignment,
   remove it from the IL.  */

static void
remove_stmt_or_phi (gimple stmt)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);

  if (gimple_code (stmt) == GIMPLE_PHI)
    remove_phi_node (&gsi, true);
  else
    {
      gsi_remove (&gsi, true);
      release_defs (stmt);
    }
}

/* Given a statement STMT, which is either a PHI node or an assignment,
   return the "rhs" of the node, in the case of a non-degenerate
   phi, NULL is returned.  */

static tree
get_rhs_or_phi_arg (gimple stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return degenerate_phi_result (stmt);
  else if (gimple_assign_single_p (stmt))
    return gimple_assign_rhs1 (stmt);
  else
    gcc_unreachable ();
}


/* Given a statement STMT, which is either a PHI node or an assignment,
   return the "lhs" of the node.  */

static tree
get_lhs_or_phi_result (gimple stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return gimple_phi_result (stmt);
  else if (is_gimple_assign (stmt))
    return gimple_assign_lhs (stmt);
  else
    gcc_unreachable ();
}

/* Propagate RHS into all uses of LHS (when possible).

   RHS and LHS are derived from STMT, which is passed in solely so
   that we can remove it if propagation is successful.

   When propagating into a PHI node or into a statement which turns
   into a trivial copy or constant initialization, set the
   appropriate bit in INTERESTING_NAMEs so that we will visit those
   nodes as well in an effort to pick up secondary optimization
   opportunities.  */

static void
propagate_rhs_into_lhs (gimple stmt, tree lhs, tree rhs, bitmap interesting_names)
{
  /* First verify that propagation is valid and isn't going to move a
     loop variant variable outside its loop.  */
  if (! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
      && (TREE_CODE (rhs) != SSA_NAME
	  || ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs))
      && may_propagate_copy (lhs, rhs)
      && loop_depth_of_name (lhs) >= loop_depth_of_name (rhs))
    {
      use_operand_p use_p;
      imm_use_iterator iter;
      gimple use_stmt;
      bool all = true;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replacing '");
	  print_generic_expr (dump_file, lhs, dump_flags);
	  fprintf (dump_file, "' with %s '",
	           (TREE_CODE (rhs) != SSA_NAME ? "constant" : "variable"));
		   print_generic_expr (dump_file, rhs, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      /* Walk over every use of LHS and try to replace the use with RHS.
	 At this point the only reason why such a propagation would not
	 be successful would be if the use occurs in an ASM_EXPR.  */
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	{
	  /* Leave debug stmts alone.  If we succeed in propagating
	     all non-debug uses, we'll drop the DEF, and propagation
	     into debug stmts will occur then.  */
	  if (gimple_debug_bind_p (use_stmt))
	    continue;

	  /* It's not always safe to propagate into an ASM_EXPR.  */
	  if (gimple_code (use_stmt) == GIMPLE_ASM
              && ! may_propagate_copy_into_asm (lhs))
	    {
	      all = false;
	      continue;
	    }

	  /* It's not ok to propagate into the definition stmt of RHS.
		<bb 9>:
		  # prephitmp.12_36 = PHI <g_67.1_6(9)>
		  g_67.1_6 = prephitmp.12_36;
		  goto <bb 9>;
	     While this is strictly all dead code we do not want to
	     deal with this here.  */
	  if (TREE_CODE (rhs) == SSA_NAME
	      && SSA_NAME_DEF_STMT (rhs) == use_stmt)
	    {
	      all = false;
	      continue;
	    }

	  /* Dump details.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    Original statement:");
	      print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
	    }

	  /* Propagate the RHS into this use of the LHS.  */
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	    propagate_value (use_p, rhs);

	  /* Special cases to avoid useless calls into the folding
	     routines, operand scanning, etc.

	     Propagation into a PHI may cause the PHI to become
	     a degenerate, so mark the PHI as interesting.  No other
	     actions are necessary.  */
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      tree result;

	      /* Dump details.  */
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "    Updated statement:");
		  print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
		}

	      result = get_lhs_or_phi_result (use_stmt);
	      bitmap_set_bit (interesting_names, SSA_NAME_VERSION (result));
	      continue;
	    }

	  /* From this point onward we are propagating into a
	     real statement.  Folding may (or may not) be possible,
	     we may expose new operands, expose dead EH edges,
	     etc.  */
          /* NOTE tuples. In the tuples world, fold_stmt_inplace
             cannot fold a call that simplifies to a constant,
             because the GIMPLE_CALL must be replaced by a
             GIMPLE_ASSIGN, and there is no way to effect such a
             transformation in-place.  We might want to consider
             using the more general fold_stmt here.  */
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	      fold_stmt_inplace (&gsi);
	    }

	  /* Sometimes propagation can expose new operands to the
	     renamer.  */
	  update_stmt (use_stmt);

	  /* Dump details.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    Updated statement:");
	      print_gimple_stmt (dump_file, use_stmt, 0, dump_flags);
	    }

	  /* If we replaced a variable index with a constant, then
	     we would need to update the invariant flag for ADDR_EXPRs.  */
          if (gimple_assign_single_p (use_stmt)
              && TREE_CODE (gimple_assign_rhs1 (use_stmt)) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr
                (gimple_assign_rhs1 (use_stmt));

	  /* If we cleaned up EH information from the statement,
	     mark its containing block as needing EH cleanups.  */
	  if (maybe_clean_or_replace_eh_stmt (use_stmt, use_stmt))
	    {
	      bitmap_set_bit (need_eh_cleanup, gimple_bb (use_stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Flagged to clear EH edges.\n");
	    }

	  /* Propagation may expose new trivial copy/constant propagation
	     opportunities.  */
          if (gimple_assign_single_p (use_stmt)
              && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
              && (TREE_CODE (gimple_assign_rhs1 (use_stmt)) == SSA_NAME
                  || is_gimple_min_invariant (gimple_assign_rhs1 (use_stmt))))
            {
	      tree result = get_lhs_or_phi_result (use_stmt);
	      bitmap_set_bit (interesting_names, SSA_NAME_VERSION (result));
	    }

	  /* Propagation into these nodes may make certain edges in
	     the CFG unexecutable.  We want to identify them as PHI nodes
	     at the destination of those unexecutable edges may become
	     degenerates.  */
	  else if (gimple_code (use_stmt) == GIMPLE_COND
		   || gimple_code (use_stmt) == GIMPLE_SWITCH
		   || gimple_code (use_stmt) == GIMPLE_GOTO)
            {
	      tree val;

	      if (gimple_code (use_stmt) == GIMPLE_COND)
                val = fold_binary_loc (gimple_location (use_stmt),
				   gimple_cond_code (use_stmt),
                                   boolean_type_node,
                                   gimple_cond_lhs (use_stmt),
                                   gimple_cond_rhs (use_stmt));
              else if (gimple_code (use_stmt) == GIMPLE_SWITCH)
		val = gimple_switch_index (use_stmt);
	      else
		val = gimple_goto_dest  (use_stmt);

	      if (val && is_gimple_min_invariant (val))
		{
		  basic_block bb = gimple_bb (use_stmt);
		  edge te = find_taken_edge (bb, val);
		  edge_iterator ei;
		  edge e;
		  gimple_stmt_iterator gsi, psi;

		  /* Remove all outgoing edges except TE.  */
		  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei));)
		    {
		      if (e != te)
			{
			  /* Mark all the PHI nodes at the destination of
			     the unexecutable edge as interesting.  */
                          for (psi = gsi_start_phis (e->dest);
                               !gsi_end_p (psi);
                               gsi_next (&psi))
                            {
                              gimple phi = gsi_stmt (psi);

			      tree result = gimple_phi_result (phi);
			      int version = SSA_NAME_VERSION (result);

			      bitmap_set_bit (interesting_names, version);
			    }

			  te->probability += e->probability;

			  te->count += e->count;
			  remove_edge (e);
			  cfg_altered = true;
			}
		      else
			ei_next (&ei);
		    }

		  gsi = gsi_last_bb (gimple_bb (use_stmt));
		  gsi_remove (&gsi, true);

		  /* And fixup the flags on the single remaining edge.  */
		  te->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
		  te->flags &= ~EDGE_ABNORMAL;
		  te->flags |= EDGE_FALLTHRU;
		  if (te->probability > REG_BR_PROB_BASE)
		    te->probability = REG_BR_PROB_BASE;
	        }
	    }
	}

      /* Ensure there is nothing else to do. */
      gcc_assert (!all || has_zero_uses (lhs));

      /* If we were able to propagate away all uses of LHS, then
	 we can remove STMT.  */
      if (all)
	remove_stmt_or_phi (stmt);
    }
}

/* STMT is either a PHI node (potentially a degenerate PHI node) or
   a statement that is a trivial copy or constant initialization.

   Attempt to eliminate T by propagating its RHS into all uses of
   its LHS.  This may in turn set new bits in INTERESTING_NAMES
   for nodes we want to revisit later.

   All exit paths should clear INTERESTING_NAMES for the result
   of STMT.  */

static void
eliminate_const_or_copy (gimple stmt, bitmap interesting_names)
{
  tree lhs = get_lhs_or_phi_result (stmt);
  tree rhs;
  int version = SSA_NAME_VERSION (lhs);

  /* If the LHS of this statement or PHI has no uses, then we can
     just eliminate it.  This can occur if, for example, the PHI
     was created by block duplication due to threading and its only
     use was in the conditional at the end of the block which was
     deleted.  */
  if (has_zero_uses (lhs))
    {
      bitmap_clear_bit (interesting_names, version);
      remove_stmt_or_phi (stmt);
      return;
    }

  /* Get the RHS of the assignment or PHI node if the PHI is a
     degenerate.  */
  rhs = get_rhs_or_phi_arg (stmt);
  if (!rhs)
    {
      bitmap_clear_bit (interesting_names, version);
      return;
    }

  if (!virtual_operand_p (lhs))
    propagate_rhs_into_lhs (stmt, lhs, rhs, interesting_names);
  else
    {
      gimple use_stmt;
      imm_use_iterator iter;
      use_operand_p use_p;
      /* For virtual operands we have to propagate into all uses as
         otherwise we will create overlapping life-ranges.  */
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	  SET_USE (use_p, rhs);
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs) = 1;
      remove_stmt_or_phi (stmt);
    }

  /* Note that STMT may well have been deleted by now, so do
     not access it, instead use the saved version # to clear
     T's entry in the worklist.  */
  bitmap_clear_bit (interesting_names, version);
}

/* The first phase in degenerate PHI elimination.

   Eliminate the degenerate PHIs in BB, then recurse on the
   dominator children of BB.  */

static void
eliminate_degenerate_phis_1 (basic_block bb, bitmap interesting_names)
{
  gimple_stmt_iterator gsi;
  basic_block son;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);

      eliminate_const_or_copy (phi, interesting_names);
    }

  /* Recurse into the dominator children of BB.  */
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    eliminate_degenerate_phis_1 (son, interesting_names);
}


/* A very simple pass to eliminate degenerate PHI nodes from the
   IL.  This is meant to be fast enough to be able to be run several
   times in the optimization pipeline.

   Certain optimizations, particularly those which duplicate blocks
   or remove edges from the CFG can create or expose PHIs which are
   trivial copies or constant initializations.

   While we could pick up these optimizations in DOM or with the
   combination of copy-prop and CCP, those solutions are far too
   heavy-weight for our needs.

   This implementation has two phases so that we can efficiently
   eliminate the first order degenerate PHIs and second order
   degenerate PHIs.

   The first phase performs a dominator walk to identify and eliminate
   the vast majority of the degenerate PHIs.  When a degenerate PHI
   is identified and eliminated any affected statements or PHIs
   are put on a worklist.

   The second phase eliminates degenerate PHIs and trivial copies
   or constant initializations using the worklist.  This is how we
   pick up the secondary optimization opportunities with minimal
   cost.  */

namespace {

const pass_data pass_data_phi_only_cprop =
{
  GIMPLE_PASS, /* type */
  "phicprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TREE_PHI_CPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_phi_only_cprop : public gimple_opt_pass
{
public:
  pass_phi_only_cprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_phi_only_cprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_phi_only_cprop (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dom != 0; }
  virtual unsigned int execute (function *);

}; // class pass_phi_only_cprop

unsigned int
pass_phi_only_cprop::execute (function *fun)
{
  bitmap interesting_names;
  bitmap interesting_names1;

  /* Bitmap of blocks which need EH information updated.  We can not
     update it on-the-fly as doing so invalidates the dominator tree.  */
  need_eh_cleanup = BITMAP_ALLOC (NULL);

  /* INTERESTING_NAMES is effectively our worklist, indexed by
     SSA_NAME_VERSION.

     A set bit indicates that the statement or PHI node which
     defines the SSA_NAME should be (re)examined to determine if
     it has become a degenerate PHI or trivial const/copy propagation
     opportunity.

     Experiments have show we generally get better compilation
     time behavior with bitmaps rather than sbitmaps.  */
  interesting_names = BITMAP_ALLOC (NULL);
  interesting_names1 = BITMAP_ALLOC (NULL);

  calculate_dominance_info (CDI_DOMINATORS);
  cfg_altered = false;

  /* First phase.  Eliminate degenerate PHIs via a dominator
     walk of the CFG.

     Experiments have indicated that we generally get better
     compile-time behavior by visiting blocks in the first
     phase in dominator order.  Presumably this is because walking
     in dominator order leaves fewer PHIs for later examination
     by the worklist phase.  */
  eliminate_degenerate_phis_1 (ENTRY_BLOCK_PTR_FOR_FN (fun),
			       interesting_names);

  /* Second phase.  Eliminate second order degenerate PHIs as well
     as trivial copies or constant initializations identified by
     the first phase or this phase.  Basically we keep iterating
     until our set of INTERESTING_NAMEs is empty.   */
  while (!bitmap_empty_p (interesting_names))
    {
      unsigned int i;
      bitmap_iterator bi;

      /* EXECUTE_IF_SET_IN_BITMAP does not like its bitmap
	 changed during the loop.  Copy it to another bitmap and
	 use that.  */
      bitmap_copy (interesting_names1, interesting_names);

      EXECUTE_IF_SET_IN_BITMAP (interesting_names1, 0, i, bi)
	{
	  tree name = ssa_name (i);

	  /* Ignore SSA_NAMEs that have been released because
	     their defining statement was deleted (unreachable).  */
	  if (name)
	    eliminate_const_or_copy (SSA_NAME_DEF_STMT (ssa_name (i)),
				     interesting_names);
	}
    }

  if (cfg_altered)
    {
      free_dominance_info (CDI_DOMINATORS);
      /* If we changed the CFG schedule loops for fixup by cfgcleanup.  */
      if (current_loops)
	loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Propagation of const and copies may make some EH edges dead.  Purge
     such edges from the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      BITMAP_FREE (need_eh_cleanup);
    }

  BITMAP_FREE (interesting_names);
  BITMAP_FREE (interesting_names1);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_phi_only_cprop (gcc::context *ctxt)
{
  return new pass_phi_only_cprop (ctxt);
}
