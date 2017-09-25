/* Generate pattern matching and transform code shared between
   GENERIC and GIMPLE folding code from match-and-simplify description.

   Copyright (C) 2014-2015 Free Software Foundation, Inc.
   Contributed by Richard Biener <rguenther@suse.de>
   and Prathamesh Kulkarni  <bilbotheelffriend@gmail.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "ggc.h"
#include <cpplib.h>
#include "errors.h"
#include "hashtab.h"
#include "hash-table.h"
#include "hash-map.h"
#include "hash-set.h"
#include "vec.h"
#include "is-a.h"


/* Stubs for GGC referenced through instantiations triggered by hash-map.  */
void *ggc_internal_cleared_alloc (size_t, void (*)(void *),
				  size_t, size_t MEM_STAT_DECL)
{
  return NULL;
}
void ggc_free (void *)
{
}


/* libccp helpers.  */

static struct line_maps *line_table;

static bool
#if GCC_VERSION >= 4001
__attribute__((format (printf, 6, 0)))
#endif
error_cb (cpp_reader *, int errtype, int, source_location location,
	  unsigned int, const char *msg, va_list *ap)
{
  const line_map *map;
  linemap_resolve_location (line_table, location, LRK_SPELLING_LOCATION, &map);
  expanded_location loc = linemap_expand_location (line_table, map, location);
  fprintf (stderr, "%s:%d:%d %s: ", loc.file, loc.line, loc.column,
	   (errtype == CPP_DL_WARNING) ? "warning" : "error");
  vfprintf (stderr, msg, *ap);
  fprintf (stderr, "\n");
  FILE *f = fopen (loc.file, "r");
  if (f)
    {
      char buf[128];
      while (loc.line > 0)
	{
	  if (!fgets (buf, 128, f))
	    goto notfound;
	  if (buf[strlen (buf) - 1] != '\n')
	    {
	      if (loc.line > 1)
		loc.line++;
	    }
	  loc.line--;
	}
      fprintf (stderr, "%s", buf);
      for (int i = 0; i < loc.column - 1; ++i)
	fputc (' ', stderr);
      fputc ('^', stderr);
      fputc ('\n', stderr);
notfound:
      fclose (f);
    }

  if (errtype == CPP_DL_FATAL)
    exit (1);
  return false;
}

static void
#if GCC_VERSION >= 4001
__attribute__((format (printf, 2, 3)))
#endif
fatal_at (const cpp_token *tk, const char *msg, ...)
{
  va_list ap;
  va_start (ap, msg);
  error_cb (NULL, CPP_DL_FATAL, 0, tk->src_loc, 0, msg, &ap);
  va_end (ap);
}

static void
#if GCC_VERSION >= 4001
__attribute__((format (printf, 2, 3)))
#endif
fatal_at (source_location loc, const char *msg, ...)
{
  va_list ap;
  va_start (ap, msg);
  error_cb (NULL, CPP_DL_FATAL, 0, loc, 0, msg, &ap);
  va_end (ap);
}

static void
#if GCC_VERSION >= 4001
__attribute__((format (printf, 2, 3)))
#endif
warning_at (const cpp_token *tk, const char *msg, ...)
{
  va_list ap;
  va_start (ap, msg);
  error_cb (NULL, CPP_DL_WARNING, 0, tk->src_loc, 0, msg, &ap);
  va_end (ap);
}

static void
output_line_directive (FILE *f, source_location location,
		       bool dumpfile = false)
{
  const line_map *map;
  linemap_resolve_location (line_table, location, LRK_SPELLING_LOCATION, &map);
  expanded_location loc = linemap_expand_location (line_table, map, location);
  if (dumpfile)
    {
      /* When writing to a dumpfile only dump the filename.  */
      const char *file = strrchr (loc.file, DIR_SEPARATOR);
      if (!file)
	file = loc.file;
      else
	++file;
      fprintf (f, "%s:%d", file, loc.line);
    }
  else
    /* Other gen programs really output line directives here, at least for
       development it's right now more convenient to have line information
       from the generated file.  Still keep the directives as comment for now
       to easily back-point to the meta-description.  */
    fprintf (f, "/* #line %d \"%s\" */\n", loc.line, loc.file);
}


/* Pull in tree codes and builtin function codes from their
   definition files.  */

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,
enum tree_code {
#include "tree.def"
CONVERT0,
CONVERT1,
CONVERT2,
MAX_TREE_CODES
};
#undef DEFTREECODE

#define DEF_BUILTIN(ENUM, N, C, T, LT, B, F, NA, AT, IM, COND) ENUM,
enum built_in_function {
#include "builtins.def"
END_BUILTINS
};
#undef DEF_BUILTIN


/* Base class for all identifiers the parser knows.  */

struct id_base : typed_noop_remove<id_base>
{
  enum id_kind { CODE, FN, PREDICATE, USER } kind;

  id_base (id_kind, const char *, int = -1);

  hashval_t hashval;
  int nargs;
  const char *id;

  /* hash_table support.  */
  typedef id_base value_type;
  typedef id_base compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
};

inline hashval_t
id_base::hash (const value_type *op)
{
  return op->hashval;
}

inline int
id_base::equal (const value_type *op1,
			const compare_type *op2)
{
  return (op1->hashval == op2->hashval
	  && strcmp (op1->id, op2->id) == 0);
}

/* Hashtable of known pattern operators.  This is pre-seeded from
   all known tree codes and all known builtin function ids.  */
static hash_table<id_base> *operators;

id_base::id_base (id_kind kind_, const char *id_, int nargs_)
{
  kind = kind_;
  id = id_;
  nargs = nargs_;
  hashval = htab_hash_string (id);
}

/* Identifier that maps to a tree code.  */

struct operator_id : public id_base
{
  operator_id (enum tree_code code_, const char *id_, unsigned nargs_,
	       const char *tcc_)
      : id_base (id_base::CODE, id_, nargs_), code (code_), tcc (tcc_) {}
  enum tree_code code;
  const char *tcc;
};

/* Identifier that maps to a builtin function code.  */

struct fn_id : public id_base
{
  fn_id (enum built_in_function fn_, const char *id_)
      : id_base (id_base::FN, id_), fn (fn_) {}
  enum built_in_function fn;
};

struct simplify;

/* Identifier that maps to a user-defined predicate.  */

struct predicate_id : public id_base
{
  predicate_id (const char *id_)
    : id_base (id_base::PREDICATE, id_), matchers (vNULL) {}
  vec<simplify *> matchers;
};

/* Identifier that maps to a operator defined by a 'for' directive.  */

struct user_id : public id_base
{
  user_id (const char *id_, bool is_oper_list_ = false)
    : id_base (id_base::USER, id_), substitutes (vNULL),
      used (false), is_oper_list (is_oper_list_) {}
  vec<id_base *> substitutes;
  bool used;
  bool is_oper_list;
};

template<>
template<>
inline bool
is_a_helper <fn_id *>::test (id_base *id)
{
  return id->kind == id_base::FN;
}

template<>
template<>
inline bool
is_a_helper <operator_id *>::test (id_base *id)
{
  return id->kind == id_base::CODE;
}

template<>
template<>
inline bool
is_a_helper <predicate_id *>::test (id_base *id)
{
  return id->kind == id_base::PREDICATE;
}

template<>
template<>
inline bool
is_a_helper <user_id *>::test (id_base *id)
{
  return id->kind == id_base::USER;
}

/* Add a predicate identifier to the hash.  */

static predicate_id *
add_predicate (const char *id)
{
  predicate_id *p = new predicate_id (id);
  id_base **slot = operators->find_slot_with_hash (p, p->hashval, INSERT);
  if (*slot)
    fatal ("duplicate id definition");
  *slot = p;
  return p;
}

/* Add a tree code identifier to the hash.  */

static void
add_operator (enum tree_code code, const char *id,
	      const char *tcc, unsigned nargs)
{
  if (strcmp (tcc, "tcc_unary") != 0
      && strcmp (tcc, "tcc_binary") != 0
      && strcmp (tcc, "tcc_comparison") != 0
      && strcmp (tcc, "tcc_expression") != 0
      /* For {REAL,IMAG}PART_EXPR and VIEW_CONVERT_EXPR.  */
      && strcmp (tcc, "tcc_reference") != 0
      /* To have INTEGER_CST and friends as "predicate operators".  */
      && strcmp (tcc, "tcc_constant") != 0
      /* And allow CONSTRUCTOR for vector initializers.  */
      && !(code == CONSTRUCTOR))
    return;
  operator_id *op = new operator_id (code, id, nargs, tcc);
  id_base **slot = operators->find_slot_with_hash (op, op->hashval, INSERT);
  if (*slot)
    fatal ("duplicate id definition");
  *slot = op;
}

/* Add a builtin identifier to the hash.  */

static void
add_builtin (enum built_in_function code, const char *id)
{
  fn_id *fn = new fn_id (code, id);
  id_base **slot = operators->find_slot_with_hash (fn, fn->hashval, INSERT);
  if (*slot)
    fatal ("duplicate id definition");
  *slot = fn;
}

/* Helper for easy comparing ID with tree code CODE.  */

static bool
operator==(id_base &id, enum tree_code code)
{
  if (operator_id *oid = dyn_cast <operator_id *> (&id))
    return oid->code == code;
  return false;
}

/* Lookup the identifier ID.  */

id_base *
get_operator (const char *id)
{
  id_base tem (id_base::CODE, id);

  id_base *op = operators->find_with_hash (&tem, tem.hashval);
  if (op)
    {
      /* If this is a user-defined identifier track whether it was used.  */
      if (user_id *uid = dyn_cast<user_id *> (op))
	uid->used = true;
      return op;
    }

  /* Try all-uppercase.  */
  char *id2 = xstrdup (id);
  for (unsigned i = 0; i < strlen (id2); ++i)
    id2[i] = TOUPPER (id2[i]);
  new (&tem) id_base (id_base::CODE, id2);
  op = operators->find_with_hash (&tem, tem.hashval);
  if (op)
    {
      free (id2);
      return op;
    }

  /* Try _EXPR appended.  */
  id2 = (char *)xrealloc (id2, strlen (id2) + sizeof ("_EXPR") + 1);
  strcat (id2, "_EXPR");
  new (&tem) id_base (id_base::CODE, id2);
  op = operators->find_with_hash (&tem, tem.hashval);
  if (op)
    {
      free (id2);
      return op;
    }

  return 0;
}


/* Helper for the capture-id map.  */

struct capture_id_map_hasher : default_hashmap_traits
{
  static inline hashval_t hash (const char *);
  static inline bool equal_keys (const char *, const char *);
};

inline hashval_t
capture_id_map_hasher::hash (const char *id)
{
  return htab_hash_string (id);
}

inline bool
capture_id_map_hasher::equal_keys (const char *id1, const char *id2)
{
  return strcmp (id1, id2) == 0;
}

typedef hash_map<const char *, unsigned, capture_id_map_hasher> cid_map_t;


/* The AST produced by parsing of the pattern definitions.  */

struct dt_operand;
struct capture_info;

/* The base class for operands.  */

struct operand {
  enum op_type { OP_PREDICATE, OP_EXPR, OP_CAPTURE, OP_C_EXPR };
  operand (enum op_type type_) : type (type_) {}
  enum op_type type;
  virtual void gen_transform (FILE *, const char *, bool, int,
			      const char *, capture_info *,
			      dt_operand ** = 0,
			      bool = true)
    { gcc_unreachable  (); }
};

/* A predicate operand.  Predicates are leafs in the AST.  */

struct predicate : public operand
{
  predicate (predicate_id *p_) : operand (OP_PREDICATE), p (p_) {}
  predicate_id *p;
};

/* An operand that constitutes an expression.  Expressions include
   function calls and user-defined predicate invocations.  */

struct expr : public operand
{
  expr (id_base *operation_, bool is_commutative_ = false)
    : operand (OP_EXPR), operation (operation_),
      ops (vNULL), expr_type (NULL), is_commutative (is_commutative_),
      is_generic (false) {}
  void append_op (operand *op) { ops.safe_push (op); }
  /* The operator and its operands.  */
  id_base *operation;
  vec<operand *> ops;
  /* An explicitely specified type - used exclusively for conversions.  */
  const char *expr_type;
  /* Whether the operation is to be applied commutatively.  This is
     later lowered to two separate patterns.  */
  bool is_commutative;
  /* Whether the expression is expected to be in GENERIC form.  */
  bool is_generic;
  virtual void gen_transform (FILE *f, const char *, bool, int,
			      const char *, capture_info *,
			      dt_operand ** = 0, bool = true);
};

/* An operator that is represented by native C code.  This is always
   a leaf operand in the AST.  This class is also used to represent
   the code to be generated for 'if' and 'with' expressions.  */

struct c_expr : public operand
{
  /* A mapping of an identifier and its replacement.  Used to apply
     'for' lowering.  */
  struct id_tab {
    const char *id;
    const char *oper;
    id_tab (const char *id_, const char *oper_): id (id_), oper (oper_) {}
  };

  c_expr (cpp_reader *r_, vec<cpp_token> code_, unsigned nr_stmts_,
	  vec<id_tab> ids_, cid_map_t *capture_ids_)
    : operand (OP_C_EXPR), r (r_), code (code_), capture_ids (capture_ids_),
      nr_stmts (nr_stmts_), ids (ids_) {}
  /* cpplib tokens and state to transform this back to source.  */
  cpp_reader *r;
  vec<cpp_token> code;
  cid_map_t *capture_ids;
  /* The number of statements parsed (well, the number of ';'s).  */
  unsigned nr_stmts;
  /* The identifier replacement vector.  */
  vec<id_tab> ids;
  virtual void gen_transform (FILE *f, const char *, bool, int,
			      const char *, capture_info *,
			      dt_operand ** = 0, bool = true);
};

/* A wrapper around another operand that captures its value.  */

struct capture : public operand
{
  capture (unsigned where_, operand *what_)
      : operand (OP_CAPTURE), where (where_), what (what_) {}
  /* Identifier index for the value.  */
  unsigned where;
  /* The captured value.  */
  operand *what;
  virtual void gen_transform (FILE *f, const char *, bool, int,
			      const char *, capture_info *,
			      dt_operand ** = 0, bool = true);
};

template<>
template<>
inline bool
is_a_helper <capture *>::test (operand *op)
{
  return op->type == operand::OP_CAPTURE;
}

template<>
template<>
inline bool
is_a_helper <predicate *>::test (operand *op)
{
  return op->type == operand::OP_PREDICATE;
}

template<>
template<>
inline bool
is_a_helper <c_expr *>::test (operand *op)
{
  return op->type == operand::OP_C_EXPR;
}

template<>
template<>
inline bool
is_a_helper <expr *>::test (operand *op)
{
  return op->type == operand::OP_EXPR;
}

/* Helper to distinguish 'if' from 'with' expressions.  */

struct if_or_with
{
  if_or_with (operand *cexpr_, source_location location_, bool is_with_)
      : location (location_), cexpr (cexpr_), is_with (is_with_) {}
  source_location location;
  operand *cexpr;
  bool is_with;
};

/* The main class of a pattern and its transform.  This is used to
   represent both (simplify ...) and (match ...) kinds.  The AST
   duplicates all outer 'if' and 'for' expressions here so each
   simplify can exist in isolation.  */

struct simplify
{
  simplify (operand *match_, source_location match_location_,
	    struct operand *result_, source_location result_location_,
	    vec<if_or_with> ifexpr_vec_, vec<vec<user_id *> > for_vec_,
	    cid_map_t *capture_ids_)
      : match (match_), match_location (match_location_),
      result (result_), result_location (result_location_),
      ifexpr_vec (ifexpr_vec_), for_vec (for_vec_),
      capture_ids (capture_ids_), capture_max (capture_ids_->elements () - 1) {}

  /* The expression that is matched against the GENERIC or GIMPLE IL.  */
  operand *match;
  source_location match_location;
  /* For a (simplify ...) the expression produced when the pattern applies.
     For a (match ...) either NULL if it is a simple predicate or the
     single expression specifying the matched operands.  */
  struct operand *result;
  source_location result_location;
  /* Collected 'if' expressions that need to evaluate to true to make
     the pattern apply.  */
  vec<if_or_with> ifexpr_vec;
  /* Collected 'for' expression operators that have to be replaced
     in the lowering phase.  */
  vec<vec<user_id *> > for_vec;
  /* A map of capture identifiers to indexes.  */
  cid_map_t *capture_ids;
  int capture_max;
};

/* Debugging routines for dumping the AST.  */

DEBUG_FUNCTION void
print_operand (operand *o, FILE *f = stderr, bool flattened = false)
{
  if (capture *c = dyn_cast<capture *> (o))
    {
      fprintf (f, "@%u", c->where);
      if (c->what && flattened == false)
	{
	  putc (':', f);
	  print_operand (c->what, f, flattened);
	  putc (' ', f);
	}
    }

  else if (predicate *p = dyn_cast<predicate *> (o))
    fprintf (f, "%s", p->p->id);

  else if (is_a<c_expr *> (o))
    fprintf (f, "c_expr");

  else if (expr *e = dyn_cast<expr *> (o))
    {
      fprintf (f, "(%s", e->operation->id);

      if (flattened == false)
	{
	  putc (' ', f);
	  for (unsigned i = 0; i < e->ops.length (); ++i)
	    {
	      print_operand (e->ops[i], f, flattened);
	      putc (' ', f);
	    }
	}
      putc (')', f);
    }

  else
    gcc_unreachable ();
}

DEBUG_FUNCTION void
print_matches (struct simplify *s, FILE *f = stderr)
{
  fprintf (f, "for expression: ");
  print_operand (s->match, f);
  putc ('\n', f);
}


/* AST lowering.  */

/* Lowering of commutative operators.  */

static void
cartesian_product (const vec< vec<operand *> >& ops_vector,
		   vec< vec<operand *> >& result, vec<operand *>& v, unsigned n)
{
  if (n == ops_vector.length ())
    {
      vec<operand *> xv = v.copy ();
      result.safe_push (xv);
      return;
    }

  for (unsigned i = 0; i < ops_vector[n].length (); ++i)
    {
      v[n] = ops_vector[n][i];
      cartesian_product (ops_vector, result, v, n + 1);
    }
}

/* Lower OP to two operands in case it is marked as commutative.  */

static vec<operand *>
commutate (operand *op)
{
  vec<operand *> ret = vNULL;

  if (capture *c = dyn_cast <capture *> (op))
    {
      if (!c->what)
	{
	  ret.safe_push (op);
	  return ret;
	}
      vec<operand *> v = commutate (c->what);
      for (unsigned i = 0; i < v.length (); ++i)
	{
	  capture *nc = new capture (c->where, v[i]);
	  ret.safe_push (nc);
	}
      return ret;
    }

  expr *e = dyn_cast <expr *> (op);
  if (!e || e->ops.length () == 0)
    {
      ret.safe_push (op);
      return ret;
    }

  vec< vec<operand *> > ops_vector = vNULL;
  for (unsigned i = 0; i < e->ops.length (); ++i)
    ops_vector.safe_push (commutate (e->ops[i]));

  auto_vec< vec<operand *> > result;
  auto_vec<operand *> v (e->ops.length ());
  v.quick_grow_cleared (e->ops.length ());
  cartesian_product (ops_vector, result, v, 0);


  for (unsigned i = 0; i < result.length (); ++i)
    {
      expr *ne = new expr (e->operation);
      for (unsigned j = 0; j < result[i].length (); ++j)
	ne->append_op (result[i][j]);
      ret.safe_push (ne);
    }

  if (!e->is_commutative)
    return ret;

  for (unsigned i = 0; i < result.length (); ++i)
    {
      expr *ne = new expr (e->operation);
      // result[i].length () is 2 since e->operation is binary
      for (unsigned j = result[i].length (); j; --j)
	ne->append_op (result[i][j-1]);
      ret.safe_push (ne);
    }

  return ret;
}

/* Lower operations marked as commutative in the AST of S and push
   the resulting patterns to SIMPLIFIERS.  */

static void
lower_commutative (simplify *s, vec<simplify *>& simplifiers)
{
  vec<operand *> matchers = commutate (s->match);
  for (unsigned i = 0; i < matchers.length (); ++i)
    {
      simplify *ns = new simplify (matchers[i], s->match_location,
				   s->result, s->result_location, s->ifexpr_vec,
				   s->for_vec, s->capture_ids);
      simplifiers.safe_push (ns);
    }
}

/* Strip conditional conversios using operator OPER from O and its
   children if STRIP, else replace them with an unconditional convert.  */

operand *
lower_opt_convert (operand *o, enum tree_code oper, bool strip)
{
  if (capture *c = dyn_cast<capture *> (o))
    {
      if (c->what)
	return new capture (c->where, lower_opt_convert (c->what, oper, strip));
      else
	return c;
    }

  expr *e = dyn_cast<expr *> (o);
  if (!e)
    return o;

  if (*e->operation == oper)
    {
      if (strip)
	return lower_opt_convert (e->ops[0], oper, strip);

      expr *ne = new expr (get_operator ("CONVERT_EXPR"));
      ne->append_op (lower_opt_convert (e->ops[0], oper, strip));
      return ne;
    }

  expr *ne = new expr (e->operation, e->is_commutative);
  for (unsigned i = 0; i < e->ops.length (); ++i)
    ne->append_op (lower_opt_convert (e->ops[i], oper, strip));

  return ne;
}

/* Determine whether O or its children uses the conditional conversion
   operator OPER.  */

static bool
has_opt_convert (operand *o, enum tree_code oper)
{
  if (capture *c = dyn_cast<capture *> (o))
    {
      if (c->what)
	return has_opt_convert (c->what, oper);
      else
	return false;
    }

  expr *e = dyn_cast<expr *> (o);
  if (!e)
    return false;

  if (*e->operation == oper)
    return true;

  for (unsigned i = 0; i < e->ops.length (); ++i)
    if (has_opt_convert (e->ops[i], oper))
      return true;

  return false;
}

/* Lower conditional convert operators in O, expanding it to a vector
   if required.  */

static vec<operand *>
lower_opt_convert (operand *o)
{
  vec<operand *> v1 = vNULL, v2;

  v1.safe_push (o);

  enum tree_code opers[] = { CONVERT0, CONVERT1, CONVERT2 };

  /* Conditional converts are lowered to a pattern with the
     conversion and one without.  The three different conditional
     convert codes are lowered separately.  */

  for (unsigned i = 0; i < 3; ++i)
    {
      v2 = vNULL;
      for (unsigned j = 0; j < v1.length (); ++j)
	if (has_opt_convert (v1[j], opers[i]))
	  {
	    v2.safe_push (lower_opt_convert (v1[j], opers[i], false));
	    v2.safe_push (lower_opt_convert (v1[j], opers[i], true));
	  }

      if (v2 != vNULL)
	{
	  v1 = vNULL;
	  for (unsigned j = 0; j < v2.length (); ++j)
	    v1.safe_push (v2[j]);
	}
    }

  return v1;
}

/* Lower conditional convert operators in the AST of S and push
   the resulting multiple patterns to SIMPLIFIERS.  */

static void
lower_opt_convert (simplify *s, vec<simplify *>& simplifiers)
{
  vec<operand *> matchers = lower_opt_convert (s->match);
  for (unsigned i = 0; i < matchers.length (); ++i)
    {
      simplify *ns = new simplify (matchers[i], s->match_location,
				   s->result, s->result_location, s->ifexpr_vec,
				   s->for_vec, s->capture_ids);
      simplifiers.safe_push (ns);
    }
}

/* Lower the compare operand of COND_EXPRs and VEC_COND_EXPRs to a
   GENERIC and a GIMPLE variant.  */

static vec<operand *>
lower_cond (operand *o)
{
  vec<operand *> ro = vNULL;

  if (capture *c = dyn_cast<capture *> (o))
    {
      if (c->what)
	{
	  vec<operand *> lop = vNULL;
	  lop = lower_cond (c->what);

	  for (unsigned i = 0; i < lop.length (); ++i)
	    ro.safe_push (new capture (c->where, lop[i]));
	  return ro;
	}
    }

  expr *e = dyn_cast<expr *> (o);
  if (!e || e->ops.length () == 0)
    {
      ro.safe_push (o);
      return ro;
    }

  vec< vec<operand *> > ops_vector = vNULL;
  for (unsigned i = 0; i < e->ops.length (); ++i)
    ops_vector.safe_push (lower_cond (e->ops[i]));

  auto_vec< vec<operand *> > result;
  auto_vec<operand *> v (e->ops.length ());
  v.quick_grow_cleared (e->ops.length ());
  cartesian_product (ops_vector, result, v, 0);

  for (unsigned i = 0; i < result.length (); ++i)
    {
      expr *ne = new expr (e->operation);
      for (unsigned j = 0; j < result[i].length (); ++j)
	ne->append_op (result[i][j]);
      ro.safe_push (ne);
      /* If this is a COND with a captured expression or an
         expression with two operands then also match a GENERIC
	 form on the compare.  */
      if ((*e->operation == COND_EXPR
	   || *e->operation == VEC_COND_EXPR)
	  && ((is_a <capture *> (e->ops[0])
	       && as_a <capture *> (e->ops[0])->what
	       && is_a <expr *> (as_a <capture *> (e->ops[0])->what)
	       && as_a <expr *>
	            (as_a <capture *> (e->ops[0])->what)->ops.length () == 2)
	      || (is_a <expr *> (e->ops[0])
		  && as_a <expr *> (e->ops[0])->ops.length () == 2)))
	{
	  expr *ne = new expr (e->operation);
	  for (unsigned j = 0; j < result[i].length (); ++j)
	    ne->append_op (result[i][j]);
	  if (capture *c = dyn_cast <capture *> (ne->ops[0]))
	    {
	      expr *ocmp = as_a <expr *> (c->what);
	      expr *cmp = new expr (ocmp->operation);
	      for (unsigned j = 0; j < ocmp->ops.length (); ++j)
		cmp->append_op (ocmp->ops[j]);
	      cmp->is_generic = true;
	      ne->ops[0] = new capture (c->where, cmp);
	    }
	  else
	    {
	      expr *ocmp = as_a <expr *> (ne->ops[0]);
	      expr *cmp = new expr (ocmp->operation);
	      for (unsigned j = 0; j < ocmp->ops.length (); ++j)
		cmp->append_op (ocmp->ops[j]);
	      cmp->is_generic = true;
	      ne->ops[0] = cmp;
	    }
	  ro.safe_push (ne);
	}
    }

  return ro;
}

/* Lower the compare operand of COND_EXPRs and VEC_COND_EXPRs to a
   GENERIC and a GIMPLE variant.  */

static void
lower_cond (simplify *s, vec<simplify *>& simplifiers)
{
  vec<operand *> matchers = lower_cond (s->match);
  for (unsigned i = 0; i < matchers.length (); ++i)
    {
      simplify *ns = new simplify (matchers[i], s->match_location,
				   s->result, s->result_location, s->ifexpr_vec,
				   s->for_vec, s->capture_ids);
      simplifiers.safe_push (ns);
    }
}

/* In AST operand O replace operator ID with operator WITH.  */

operand *
replace_id (operand *o, user_id *id, id_base *with)
{
  /* Deep-copy captures and expressions, replacing operations as
     needed.  */
  if (capture *c = dyn_cast<capture *> (o))
    {
      if (!c->what)
	return c;
      return new capture (c->where, replace_id (c->what, id, with));
    }
  else if (expr *e = dyn_cast<expr *> (o))
    {
      expr *ne = new expr (e->operation == id ? with : e->operation,
			   e->is_commutative);
      ne->expr_type = e->expr_type;
      for (unsigned i = 0; i < e->ops.length (); ++i)
	ne->append_op (replace_id (e->ops[i], id, with));
      return ne;
    }

  /* For c_expr we simply record a string replacement table which is
     applied at code-generation time.  */
  if (c_expr *ce = dyn_cast<c_expr *> (o))
    {
      vec<c_expr::id_tab> ids = ce->ids.copy ();
      ids.safe_push (c_expr::id_tab (id->id, with->id));
      return new c_expr (ce->r, ce->code, ce->nr_stmts, ids, ce->capture_ids);
    }

  return o;
}

/* Lower recorded fors for SIN and output to SIMPLIFIERS.  */

static void
lower_for (simplify *sin, vec<simplify *>& simplifiers)
{
  vec<vec<user_id *> >& for_vec = sin->for_vec;
  unsigned worklist_start = 0;
  auto_vec<simplify *> worklist;
  worklist.safe_push (sin);

  /* Lower each recorded for separately, operating on the
     set of simplifiers created by the previous one.
     Lower inner-to-outer so inner for substitutes can refer
     to operators replaced by outer fors.  */
  for (int fi = for_vec.length () - 1; fi >= 0; --fi)
    {
      vec<user_id *>& ids = for_vec[fi];
      unsigned n_ids = ids.length ();
      unsigned max_n_opers = 0;
      for (unsigned i = 0; i < n_ids; ++i)
	if (ids[i]->substitutes.length () > max_n_opers)
	  max_n_opers = ids[i]->substitutes.length ();

      unsigned worklist_end = worklist.length ();
      for (unsigned si = worklist_start; si < worklist_end; ++si)
	{
	  simplify *s = worklist[si];
	  for (unsigned j = 0; j < max_n_opers; ++j)
	    {
	      operand *match_op = s->match;
	      operand *result_op = s->result;
	      vec<if_or_with> ifexpr_vec = s->ifexpr_vec.copy ();

	      for (unsigned i = 0; i < n_ids; ++i)
		{
		  user_id *id = ids[i];
		  id_base *oper = id->substitutes[j % id->substitutes.length ()];
		  match_op = replace_id (match_op, id, oper);
		  if (result_op)
		    result_op = replace_id (result_op, id, oper);
		  for (unsigned k = 0; k < s->ifexpr_vec.length (); ++k)
		    ifexpr_vec[k].cexpr = replace_id (ifexpr_vec[k].cexpr,
						      id, oper);
		}
	      simplify *ns = new simplify (match_op, s->match_location,
					   result_op, s->result_location,
					   ifexpr_vec, vNULL, s->capture_ids);
	      worklist.safe_push (ns);
	    }
	}
      worklist_start = worklist_end;
    }

  /* Copy out the result from the last for lowering.  */
  for (unsigned i = worklist_start; i < worklist.length (); ++i)
    simplifiers.safe_push (worklist[i]);
}

/* Lower the AST for everything in SIMPLIFIERS.  */

static void
lower (vec<simplify *>& simplifiers, bool gimple)
{
  auto_vec<simplify *> out_simplifiers;
  for (unsigned i = 0; i < simplifiers.length (); ++i)
    lower_opt_convert (simplifiers[i], out_simplifiers);

  simplifiers.truncate (0);
  for (unsigned i = 0; i < out_simplifiers.length (); ++i)
    lower_commutative (out_simplifiers[i], simplifiers);

  out_simplifiers.truncate (0);
  if (gimple)
    for (unsigned i = 0; i < simplifiers.length (); ++i)
      lower_cond (simplifiers[i], out_simplifiers);
  else
    out_simplifiers.safe_splice (simplifiers);


  simplifiers.truncate (0);
  for (unsigned i = 0; i < out_simplifiers.length (); ++i)
    lower_for (out_simplifiers[i], simplifiers);
}




/* The decision tree built for generating GIMPLE and GENERIC pattern
   matching code.  It represents the 'match' expression of all
   simplifies and has those as its leafs.  */

/* Decision tree base class, used for DT_TRUE and DT_NODE.  */

struct dt_node
{
  enum dt_type { DT_NODE, DT_OPERAND, DT_TRUE, DT_MATCH, DT_SIMPLIFY };

  enum dt_type type;
  unsigned level;
  vec<dt_node *> kids;

  dt_node (enum dt_type type_): type (type_), level (0), kids (vNULL) {}

  dt_node *append_node (dt_node *);
  dt_node *append_op (operand *, dt_node *parent = 0, unsigned pos = 0);
  dt_node *append_true_op (dt_node *parent = 0, unsigned pos = 0);
  dt_node *append_match_op (dt_operand *, dt_node *parent = 0, unsigned pos = 0);
  dt_node *append_simplify (simplify *, unsigned, dt_operand **);

  virtual void gen (FILE *, bool) {}

  void gen_kids (FILE *, bool);
  void gen_kids_1 (FILE *, bool,
		   vec<dt_operand *>, vec<dt_operand *>, vec<dt_operand *>,
		   vec<dt_operand *>, vec<dt_operand *>, vec<dt_node *>);
};

/* Generic decision tree node used for DT_OPERAND and DT_MATCH.  */

struct dt_operand : public dt_node
{
  operand *op;
  dt_operand *match_dop;
  dt_operand *parent;
  unsigned pos;

  dt_operand (enum dt_type type, operand *op_, dt_operand *match_dop_,
	      dt_operand *parent_ = 0, unsigned pos_ = 0)
      : dt_node (type), op (op_), match_dop (match_dop_),
      parent (parent_), pos (pos_) {}

  void gen (FILE *, bool);
  unsigned gen_predicate (FILE *, const char *, bool);
  unsigned gen_match_op (FILE *, const char *);

  unsigned gen_gimple_expr (FILE *);
  unsigned gen_generic_expr (FILE *, const char *);

  char *get_name (char *);
  void gen_opname (char *, unsigned);
};

/* Leaf node of the decision tree, used for DT_SIMPLIFY.  */

struct dt_simplify : public dt_node
{
  simplify *s;
  unsigned pattern_no;
  dt_operand **indexes;

  dt_simplify (simplify *s_, unsigned pattern_no_, dt_operand **indexes_)
	: dt_node (DT_SIMPLIFY), s (s_), pattern_no (pattern_no_),
	  indexes (indexes_)  {}

  void gen (FILE *f, bool);
};

template<>
template<>
inline bool
is_a_helper <dt_operand *>::test (dt_node *n)
{
  return (n->type == dt_node::DT_OPERAND
	  || n->type == dt_node::DT_MATCH);
}

/* A container for the actual decision tree.  */

struct decision_tree
{
  dt_node *root;

  void insert (struct simplify *, unsigned);
  void gen_gimple (FILE *f = stderr);
  void gen_generic (FILE *f = stderr);
  void print (FILE *f = stderr);

  decision_tree () { root = new dt_node (dt_node::DT_NODE); }

  static dt_node *insert_operand (dt_node *, operand *, dt_operand **indexes,
				  unsigned pos = 0, dt_node *parent = 0);
  static dt_node *find_node (vec<dt_node *>&, dt_node *);
  static bool cmp_node (dt_node *, dt_node *);
  static void print_node (dt_node *, FILE *f = stderr, unsigned = 0);
};

/* Compare two AST operands O1 and O2 and return true if they are equal.  */

bool
cmp_operand (operand *o1, operand *o2)
{
  if (!o1 || !o2 || o1->type != o2->type)
    return false;

  if (o1->type == operand::OP_PREDICATE)
    {
      predicate *p1 = as_a<predicate *>(o1);
      predicate *p2 = as_a<predicate *>(o2);
      return p1->p == p2->p;
    }
  else if (o1->type == operand::OP_EXPR)
    {
      expr *e1 = static_cast<expr *>(o1);
      expr *e2 = static_cast<expr *>(o2);
      return (e1->operation == e2->operation
	      && e1->is_generic == e2->is_generic);
    }
  else
    return false;
}

/* Compare two decision tree nodes N1 and N2 and return true if they
   are equal.  */

bool
decision_tree::cmp_node (dt_node *n1, dt_node *n2)
{
  if (!n1 || !n2 || n1->type != n2->type)
    return false;

  if (n1 == n2)
    return true;

  if (n1->type == dt_node::DT_TRUE)
    return false;

  if (n1->type == dt_node::DT_OPERAND)
    return cmp_operand ((as_a<dt_operand *> (n1))->op,
			(as_a<dt_operand *> (n2))->op);
  else if (n1->type == dt_node::DT_MATCH)
    return ((as_a<dt_operand *> (n1))->match_dop
	    == (as_a<dt_operand *> (n2))->match_dop);
  return false;
}

/* Search OPS for a decision tree node like P and return it if found.  */

dt_node *
decision_tree::find_node (vec<dt_node *>& ops, dt_node *p)
{
  /* We can merge adjacent DT_TRUE.  */
  if (p->type == dt_node::DT_TRUE
      && !ops.is_empty ()
      && ops.last ()->type == dt_node::DT_TRUE)
    return ops.last ();
  for (int i = ops.length () - 1; i >= 0; --i)
    {
      /* But we can't merge across DT_TRUE nodes as they serve as
         pattern order barriers to make sure that patterns apply
	 in order of appearance in case multiple matches are possible.  */
      if (ops[i]->type == dt_node::DT_TRUE)
	return NULL;
      if (decision_tree::cmp_node (ops[i], p))
	return ops[i];
    }
  return NULL;
}

/* Append N to the decision tree if it there is not already an existing
   identical child.  */

dt_node *
dt_node::append_node (dt_node *n)
{
  dt_node *kid;

  kid = decision_tree::find_node (kids, n);
  if (kid)
    return kid;

  kids.safe_push (n);
  n->level = this->level + 1;

  return n;
}

/* Append OP to the decision tree.  */

dt_node *
dt_node::append_op (operand *op, dt_node *parent, unsigned pos)
{
  dt_operand *parent_ = safe_as_a<dt_operand *> (parent);
  dt_operand *n = new dt_operand (DT_OPERAND, op, 0, parent_, pos);
  return append_node (n);
}

/* Append a DT_TRUE decision tree node.  */

dt_node *
dt_node::append_true_op (dt_node *parent, unsigned pos)
{
  dt_operand *parent_ = safe_as_a<dt_operand *> (parent);
  dt_operand *n = new dt_operand (DT_TRUE, 0, 0, parent_, pos);
  return append_node (n);
}

/* Append a DT_MATCH decision tree node.  */

dt_node *
dt_node::append_match_op (dt_operand *match_dop, dt_node *parent, unsigned pos)
{
  dt_operand *parent_ = as_a<dt_operand *> (parent);
  dt_operand *n = new dt_operand (DT_MATCH, 0, match_dop, parent_, pos);
  return append_node (n);
}

/* Append S to the decision tree.  */

dt_node *
dt_node::append_simplify (simplify *s, unsigned pattern_no,
			  dt_operand **indexes)
{
  dt_simplify *n = new dt_simplify (s, pattern_no, indexes);
  return append_node (n);
}

/* Insert O into the decision tree and return the decision tree node found
   or created.  */

dt_node *
decision_tree::insert_operand (dt_node *p, operand *o, dt_operand **indexes,
			       unsigned pos, dt_node *parent)
{
  dt_node *q, *elm = 0;

  if (capture *c = dyn_cast<capture *> (o))
    {
      unsigned capt_index = c->where;

      if (indexes[capt_index] == 0)
	{
	  if (c->what)
	    q = insert_operand (p, c->what, indexes, pos, parent);
	  else
	    {
	      q = elm = p->append_true_op (parent, pos);
	      goto at_assert_elm;
	    }
	  // get to the last capture
	  for (operand *what = c->what;
	       what && is_a<capture *> (what);
	       c = as_a<capture *> (what), what = c->what)
	    ;

	  if (!c->what)
	    {
	      unsigned cc_index = c->where;
	      dt_operand *match_op = indexes[cc_index];

	      dt_operand temp (dt_node::DT_TRUE, 0, 0);
	      elm = decision_tree::find_node (p->kids, &temp);

	      if (elm == 0)
		{
		  dt_operand temp (dt_node::DT_MATCH, 0, match_op);
		  elm = decision_tree::find_node (p->kids, &temp);
		}
	    }
	  else
	    {
	      dt_operand temp (dt_node::DT_OPERAND, c->what, 0);
	      elm = decision_tree::find_node (p->kids, &temp);
	    }

at_assert_elm:
	  gcc_assert (elm->type == dt_node::DT_TRUE
		      || elm->type == dt_node::DT_OPERAND
		      || elm->type == dt_node::DT_MATCH);
	  indexes[capt_index] = static_cast<dt_operand *> (elm);
	  return q;
	}
      else
	{
	  p = p->append_match_op (indexes[capt_index], parent, pos);
	  if (c->what)
	    return insert_operand (p, c->what, indexes, 0, p);
	  else
	    return p;
	}
    }
  p = p->append_op (o, parent, pos);
  q = p;

  if (expr *e = dyn_cast <expr *>(o))
    {
      for (unsigned i = 0; i < e->ops.length (); ++i)
	q = decision_tree::insert_operand (q, e->ops[i], indexes, i, p);
    }

  return q;
}

/* Insert S into the decision tree.  */

void
decision_tree::insert (struct simplify *s, unsigned pattern_no)
{
  dt_operand **indexes = XCNEWVEC (dt_operand *, s->capture_max + 1);
  dt_node *p = decision_tree::insert_operand (root, s->match, indexes);
  p->append_simplify (s, pattern_no, indexes);
}

/* Debug functions to dump the decision tree.  */

DEBUG_FUNCTION void
decision_tree::print_node (dt_node *p, FILE *f, unsigned indent)
{
  if (p->type == dt_node::DT_NODE)
    fprintf (f, "root");
  else
    {
      fprintf (f, "|");
      for (unsigned i = 0; i < indent; i++)
	fprintf (f, "-");

      if (p->type == dt_node::DT_OPERAND)
	{
	  dt_operand *dop = static_cast<dt_operand *>(p);
	  print_operand (dop->op, f, true);
	}
      else if (p->type == dt_node::DT_TRUE)
	fprintf (f, "true");
      else if (p->type == dt_node::DT_MATCH)
	fprintf (f, "match (%p)", (void *)((as_a<dt_operand *>(p))->match_dop));
      else if (p->type == dt_node::DT_SIMPLIFY)
	{
	  dt_simplify *s = static_cast<dt_simplify *> (p);
	  fprintf (f, "simplify_%u { ", s->pattern_no);
	  for (int i = 0; i <= s->s->capture_max; ++i)
	    fprintf (f, "%p, ", (void *) s->indexes[i]);
	  fprintf (f, " } ");
	}
    }

  fprintf (stderr, " (%p), %u, %u\n", (void *) p, p->level, p->kids.length ());

  for (unsigned i = 0; i < p->kids.length (); ++i)
    decision_tree::print_node (p->kids[i], f, indent + 2);
}

DEBUG_FUNCTION void
decision_tree::print (FILE *f)
{
  return decision_tree::print_node (root, f);
}


/* For GENERIC we have to take care of wrapping multiple-used
   expressions with side-effects in save_expr and preserve side-effects
   of expressions with omit_one_operand.  Analyze captures in
   match, result and with expressions and perform early-outs
   on the outermost match expression operands for cases we cannot
   handle.  */

struct capture_info
{
  capture_info (simplify *s);
  void walk_match (operand *o, unsigned toplevel_arg, bool, bool);
  void walk_result (operand *o, bool);
  void walk_c_expr (c_expr *);

  struct cinfo
    {
      bool expr_p;
      bool cse_p;
      bool force_no_side_effects_p;
      bool cond_expr_cond_p;
      unsigned long toplevel_msk;
      int result_use_count;
    };

  auto_vec<cinfo> info;
  unsigned long force_no_side_effects;
};

/* Analyze captures in S.  */

capture_info::capture_info (simplify *s)
{
  expr *e;
  if (!s->result
      || ((e = dyn_cast <expr *> (s->result))
	  && is_a <predicate_id *> (e->operation)))
    {
      force_no_side_effects = -1;
      return;
    }

  force_no_side_effects = 0;
  info.safe_grow_cleared (s->capture_max + 1);
  e = as_a <expr *> (s->match);
  for (unsigned i = 0; i < e->ops.length (); ++i)
    walk_match (e->ops[i], i,
		(i != 0 && *e->operation == COND_EXPR)
		|| *e->operation == TRUTH_ANDIF_EXPR
		|| *e->operation == TRUTH_ORIF_EXPR,
		i == 0
		&& (*e->operation == COND_EXPR
		    || *e->operation == VEC_COND_EXPR));

  walk_result (s->result, false);

  for (unsigned i = 0; i < s->ifexpr_vec.length (); ++i)
    if (s->ifexpr_vec[i].is_with)
      walk_c_expr (as_a <c_expr *>(s->ifexpr_vec[i].cexpr));
}

/* Analyze captures in the match expression piece O.  */

void
capture_info::walk_match (operand *o, unsigned toplevel_arg,
			  bool conditional_p, bool cond_expr_cond_p)
{
  if (capture *c = dyn_cast <capture *> (o))
    {
      unsigned where = c->where;
      info[where].toplevel_msk |= 1 << toplevel_arg;
      info[where].force_no_side_effects_p |= conditional_p;
      info[where].cond_expr_cond_p |= cond_expr_cond_p;
      if (!c->what)
	return;
      /* Recurse to exprs and captures.  */
      if (is_a <capture *> (c->what)
	  || is_a <expr *> (c->what))
	walk_match (c->what, toplevel_arg, conditional_p, false);
      /* We need to look past multiple captures to find a captured
	 expression as with conditional converts two captures
	 can be collapsed onto the same expression.  */
      while (c->what && is_a <capture *> (c->what))
	c = as_a <capture *> (c->what);
      /* Mark expr (non-leaf) captures.  */
      if (c->what
	  && is_a <expr *> (c->what))
	info[where].expr_p = true;
    }
  else if (expr *e = dyn_cast <expr *> (o))
    {
      for (unsigned i = 0; i < e->ops.length (); ++i)
	{
	  bool cond_p = conditional_p;
	  bool cond_expr_cond_p = false;
	  if (i != 0 && *e->operation == COND_EXPR)
	    cond_p = true;
	  else if (*e->operation == TRUTH_ANDIF_EXPR
		   || *e->operation == TRUTH_ORIF_EXPR)
	    cond_p = true;
	  if (i == 0
	      && (*e->operation == COND_EXPR
		  || *e->operation == VEC_COND_EXPR))
	    cond_expr_cond_p = true;
	  walk_match (e->ops[i], toplevel_arg, cond_p, cond_expr_cond_p);
	}
    }
  else if (is_a <predicate *> (o))
    {
      /* Mark non-captured leafs toplevel arg for checking.  */
      force_no_side_effects |= 1 << toplevel_arg;
    }
  else
    gcc_unreachable ();
}

/* Analyze captures in the result expression piece O.  */

void
capture_info::walk_result (operand *o, bool conditional_p)
{
  if (capture *c = dyn_cast <capture *> (o))
    {
      info[c->where].result_use_count++;
      /* If we substitute an expression capture we don't know
         which captures this will end up using (well, we don't
	 compute that).  Force the uses to be side-effect free
	 which means forcing the toplevels that reach the
	 expression side-effect free.  */
      if (info[c->where].expr_p)
	force_no_side_effects |= info[c->where].toplevel_msk;
      /* Mark CSE capture capture uses as forced to have
         no side-effects. */
      if (c->what
	  && is_a <expr *> (c->what))
	{
	  info[c->where].cse_p = true;
	  walk_result (c->what, true);
	}
    }
  else if (expr *e = dyn_cast <expr *> (o))
    {
      for (unsigned i = 0; i < e->ops.length (); ++i)
	{
	  bool cond_p = conditional_p;
	  if (i != 0 && *e->operation == COND_EXPR)
	    cond_p = true;
	  else if (*e->operation == TRUTH_ANDIF_EXPR
		   || *e->operation == TRUTH_ORIF_EXPR)
	    cond_p = true;
	  walk_result (e->ops[i], cond_p);
	}
    }
  else if (c_expr *e = dyn_cast <c_expr *> (o))
    walk_c_expr (e);
  else
    gcc_unreachable ();
}

/* Look for captures in the C expr E.  */

void
capture_info::walk_c_expr (c_expr *e)
{
  /* Give up for C exprs mentioning captures not inside TREE_TYPE ().  */
  unsigned p_depth = 0;
  for (unsigned i = 0; i < e->code.length (); ++i)
    {
      const cpp_token *t = &e->code[i];
      const cpp_token *n = i < e->code.length () - 1 ? &e->code[i+1] : NULL;
      if (t->type == CPP_NAME
	  && strcmp ((const char *)CPP_HASHNODE
		       (t->val.node.node)->ident.str, "TREE_TYPE") == 0
	  && n->type == CPP_OPEN_PAREN)
	p_depth++;
      else if (t->type == CPP_CLOSE_PAREN
	       && p_depth > 0)
	p_depth--;
      else if (p_depth == 0
	       && t->type == CPP_ATSIGN
	       && (n->type == CPP_NUMBER
		   || n->type == CPP_NAME)
	       && !(n->flags & PREV_WHITE))
	{
	  const char *id;
	  if (n->type == CPP_NUMBER)
	    id = (const char *)n->val.str.text;
	  else
	    id = (const char *)CPP_HASHNODE (n->val.node.node)->ident.str;
	  info[*e->capture_ids->get(id)].force_no_side_effects_p = true;
	}
    }
}


/* Code generation off the decision tree and the refered AST nodes.  */

bool
is_conversion (id_base *op)
{
  return (*op == CONVERT_EXPR
	  || *op == NOP_EXPR
	  || *op == FLOAT_EXPR
	  || *op == FIX_TRUNC_EXPR
	  || *op == VIEW_CONVERT_EXPR);
}

/* Get the type to be used for generating operands of OP from the
   various sources.  */

static const char *
get_operand_type (id_base *op, const char *in_type,
		  const char *expr_type,
		  const char *other_oprnd_type)
{
  /* Generally operands whose type does not match the type of the
     expression generated need to know their types but match and
     thus can fall back to 'other_oprnd_type'.  */
  if (is_conversion (op))
    return other_oprnd_type;
  else if (*op == REALPART_EXPR
	   || *op == IMAGPART_EXPR)
    return other_oprnd_type;
  else if (is_a <operator_id *> (op)
	   && strcmp (as_a <operator_id *> (op)->tcc, "tcc_comparison") == 0)
    return other_oprnd_type;
  else
    {
      /* Otherwise all types should match - choose one in order of
         preference.  */
      if (expr_type)
	return expr_type;
      else if (in_type)
	return in_type;
      else
	return other_oprnd_type;
    }
}

/* Generate transform code for an expression.  */

void
expr::gen_transform (FILE *f, const char *dest, bool gimple, int depth,
		     const char *in_type, capture_info *cinfo,
		     dt_operand **indexes, bool)
{
  bool conversion_p = is_conversion (operation);
  const char *type = expr_type;
  char optype[64];
  if (type)
    /* If there was a type specification in the pattern use it.  */
    ;
  else if (conversion_p)
    /* For conversions we need to build the expression using the
       outer type passed in.  */
    type = in_type;
  else if (*operation == REALPART_EXPR
	   || *operation == IMAGPART_EXPR)
    {
      /* __real and __imag use the component type of its operand.  */
      sprintf (optype, "TREE_TYPE (TREE_TYPE (ops%d[0]))", depth);
      type = optype;
    }
  else if (is_a <operator_id *> (operation)
	   && !strcmp (as_a <operator_id *> (operation)->tcc, "tcc_comparison"))
    {
      /* comparisons use boolean_type_node (or what gets in), but
         their operands need to figure out the types themselves.  */
      sprintf (optype, "boolean_type_node");
      type = optype;
    }
  else
    {
      /* Other operations are of the same type as their first operand.  */
      sprintf (optype, "TREE_TYPE (ops%d[0])", depth);
      type = optype;
    }
  if (!type)
    fatal ("two conversions in a row");

  fprintf (f, "{\n");
  fprintf (f, "  tree ops%d[%u], res;\n", depth, ops.length ());
  char op0type[64];
  snprintf (op0type, 64, "TREE_TYPE (ops%d[0])", depth);
  for (unsigned i = 0; i < ops.length (); ++i)
    {
      char dest[32];
      snprintf (dest, 32, "  ops%d[%u]", depth, i);
      const char *optype
	= get_operand_type (operation, in_type, expr_type,
			    i == 0 ? NULL : op0type);
      ops[i]->gen_transform (f, dest, gimple, depth + 1, optype, cinfo, indexes,
			     ((!(*operation == COND_EXPR)
			       && !(*operation == VEC_COND_EXPR))
			      || i != 0));
    }

  const char *opr;
  if (*operation == CONVERT_EXPR)
    opr = "NOP_EXPR";
  else
    opr = operation->id;

  if (gimple)
    {
      /* ???  Building a stmt can fail for various reasons here, seq being
         NULL or the stmt referencing SSA names occuring in abnormal PHIs.
	 So if we fail here we should continue matching other patterns.  */
      fprintf (f, "  code_helper tem_code = %s;\n"
	       "  tree tem_ops[3] = { ", opr);
      for (unsigned i = 0; i < ops.length (); ++i)
	fprintf (f, "ops%d[%u]%s", depth, i,
		 i == ops.length () - 1 ? " };\n" : ", ");
      fprintf (f, "  gimple_resimplify%d (seq, &tem_code, %s, tem_ops, valueize);\n",
	       ops.length (), type);
      fprintf (f, "  res = maybe_push_res_to_seq (tem_code, %s, tem_ops, seq);\n"
	       "  if (!res) return false;\n", type);
    }
  else
    {
      if (operation->kind == id_base::CODE)
	fprintf (f, "  res = fold_build%d_loc (loc, %s, %s",
		 ops.length(), opr, type);
      else
	fprintf (f, "  res = build_call_expr_loc (loc, "
		 "builtin_decl_implicit (%s), %d", opr, ops.length());
      for (unsigned i = 0; i < ops.length (); ++i)
	fprintf (f, ", ops%d[%u]", depth, i);
      fprintf (f, ");\n");
    }
  fprintf (f, "%s = res;\n", dest);
  fprintf (f, "}\n");
}

/* Generate code for a c_expr which is either the expression inside
   an if statement or a sequence of statements which computes a
   result to be stored to DEST.  */

void
c_expr::gen_transform (FILE *f, const char *dest,
		       bool, int, const char *, capture_info *,
		       dt_operand **, bool)
{
  if (dest && nr_stmts == 1)
    fprintf (f, "%s = ", dest);

  unsigned stmt_nr = 1;
  for (unsigned i = 0; i < code.length (); ++i)
    {
      const cpp_token *token = &code[i];

      /* Replace captures for code-gen.  */
      if (token->type == CPP_ATSIGN)
	{
	  const cpp_token *n = &code[i+1];
	  if ((n->type == CPP_NUMBER
	       || n->type == CPP_NAME)
	      && !(n->flags & PREV_WHITE))
	    {
	      if (token->flags & PREV_WHITE)
		fputc (' ', f);
	      const char *id;
	      if (n->type == CPP_NUMBER)
		id = (const char *)n->val.str.text;
	      else
		id = (const char *)CPP_HASHNODE (n->val.node.node)->ident.str;
	      fprintf (f, "captures[%u]", *capture_ids->get(id));
	      ++i;
	      continue;
	    }
	}

      if (token->flags & PREV_WHITE)
	fputc (' ', f);

      if (token->type == CPP_NAME)
	{
	  const char *id = (const char *) NODE_NAME (token->val.node.node);
	  unsigned j;
	  for (j = 0; j < ids.length (); ++j)
	    {
	    if (strcmp (id, ids[j].id) == 0)
	      {
		fprintf (f, "%s", ids[j].oper);
		break;
	      }
	    }
	  if (j < ids.length ())
	    continue;
	}

      /* Output the token as string.  */
      char *tk = (char *)cpp_token_as_text (r, token);
      fputs (tk, f);

      if (token->type == CPP_SEMICOLON)
	{
	  stmt_nr++;
	  if (dest && stmt_nr == nr_stmts)
	    fprintf (f, "\n %s = ", dest);
	  else
	    fputc ('\n', f);
	}
    }
}

/* Generate transform code for a capture.  */

void
capture::gen_transform (FILE *f, const char *dest, bool gimple, int depth,
			const char *in_type, capture_info *cinfo,
			dt_operand **indexes, bool expand_compares)
{
  if (what && is_a<expr *> (what))
    {
      if (indexes[where] == 0)
	{
	  char buf[20];
	  sprintf (buf, "captures[%u]", where);
	  what->gen_transform (f, buf, gimple, depth, in_type, cinfo, NULL);
	}
    }

  fprintf (f, "%s = captures[%u];\n", dest, where);

  /* ???  Stupid tcc_comparison GENERIC trees in COND_EXPRs.  Deal
     with substituting a capture of that.
     ???  Returning false here will also not allow any other patterns
     to match.  */
  if (gimple && expand_compares
      && cinfo->info[where].cond_expr_cond_p)
    fprintf (f, "if (COMPARISON_CLASS_P (%s))\n"
	     "  {\n"
	     "    if (!seq) return false;\n"
	     "    %s = gimple_build (seq, TREE_CODE (%s),"
	     " TREE_TYPE (%s), TREE_OPERAND (%s, 0),"
	     " TREE_OPERAND (%s, 1));\n"
	     "  }\n", dest, dest, dest, dest, dest, dest);
}

/* Return the name of the operand representing the decision tree node.
   Use NAME as space to generate it.  */

char *
dt_operand::get_name (char *name)
{
  if (!parent)
    sprintf (name, "t");
  else if (parent->level == 1)
    sprintf (name, "op%u", pos);
  else if (parent->type == dt_node::DT_MATCH)
    return parent->get_name (name);
  else
    sprintf (name, "o%u%u", parent->level, pos);
  return name;
}

/* Fill NAME with the operand name at position POS.  */

void
dt_operand::gen_opname (char *name, unsigned pos)
{
  if (!parent)
    sprintf (name, "op%u", pos);
  else
    sprintf (name, "o%u%u", level, pos);
}

/* Generate matching code for the decision tree operand which is
   a predicate.  */

unsigned
dt_operand::gen_predicate (FILE *f, const char *opname, bool gimple)
{
  predicate *p = as_a <predicate *> (op);

  if (p->p->matchers.exists ())
    {
      /* If this is a predicate generated from a pattern mangle its
	 name and pass on the valueize hook.  */
      if (gimple)
	fprintf (f, "if (gimple_%s (%s, valueize))\n", p->p->id, opname);
      else
	fprintf (f, "if (tree_%s (%s))\n", p->p->id, opname);
    }
  else
    fprintf (f, "if (%s (%s))\n", p->p->id, opname);
  fprintf (f, "{\n");
  return 1;
}

/* Generate matching code for the decision tree operand which is
   a capture-match.  */

unsigned
dt_operand::gen_match_op (FILE *f, const char *opname)
{
  char match_opname[20];
  match_dop->get_name (match_opname);
  fprintf (f, "if (%s == %s || operand_equal_p (%s, %s, 0))\n",
	   opname, match_opname, opname, match_opname);
  fprintf (f, "{\n");
  return 1;
}

/* Generate GIMPLE matching code for the decision tree operand.  */

unsigned
dt_operand::gen_gimple_expr (FILE *f)
{
  expr *e = static_cast<expr *> (op);
  id_base *id = e->operation;
  unsigned n_ops = e->ops.length ();

  for (unsigned i = 0; i < n_ops; ++i)
    {
      char child_opname[20];
      gen_opname (child_opname, i);

      if (id->kind == id_base::CODE)
	{
	  if (e->is_generic
	      || *id == REALPART_EXPR || *id == IMAGPART_EXPR
	      || *id == BIT_FIELD_REF || *id == VIEW_CONVERT_EXPR)
	    {
	      /* ???  If this is a memory operation we can't (and should not)
		 match this.  The only sensible operand types are
		 SSA names and invariants.  */
	      fprintf (f, "tree %s = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), %i);\n",
		       child_opname, i);
	      fprintf (f, "if ((TREE_CODE (%s) == SSA_NAME\n"
		       "|| is_gimple_min_invariant (%s))\n"
		       "&& (%s = do_valueize (valueize, %s)))\n"
		       "{\n", child_opname, child_opname, child_opname,
		       child_opname);
	      continue;
	    }
	  else
	    fprintf (f, "tree %s = gimple_assign_rhs%u (def_stmt);\n",
		     child_opname, i + 1);
	}
      else
	fprintf (f, "tree %s = gimple_call_arg (def_stmt, %u);\n",
		 child_opname, i);
      fprintf (f, "if ((%s = do_valueize (valueize, %s)))\n",
	       child_opname, child_opname);
      fprintf (f, "{\n");
    }

  return n_ops;
}

/* Generate GENERIC matching code for the decision tree operand.  */

unsigned
dt_operand::gen_generic_expr (FILE *f, const char *opname)
{
  expr *e = static_cast<expr *> (op);
  unsigned n_ops = e->ops.length ();

  for (unsigned i = 0; i < n_ops; ++i)
    {
      char child_opname[20];
      gen_opname (child_opname, i);

      if (e->operation->kind == id_base::CODE)
	fprintf (f, "tree %s = TREE_OPERAND (%s, %u);\n",
		 child_opname, opname, i);
      else
	fprintf (f, "tree %s = CALL_EXPR_ARG (%s, %u);\n",
		 child_opname, opname, i);
    }

  return 0;
}

/* Generate matching code for the children of the decision tree node.  */

void
dt_node::gen_kids (FILE *f, bool gimple)
{
  auto_vec<dt_operand *> gimple_exprs;
  auto_vec<dt_operand *> generic_exprs;
  auto_vec<dt_operand *> fns;
  auto_vec<dt_operand *> generic_fns;
  auto_vec<dt_operand *> preds;
  auto_vec<dt_node *> others;

  for (unsigned i = 0; i < kids.length (); ++i)
    {
      if (kids[i]->type == dt_node::DT_OPERAND)
	{
	  dt_operand *op = as_a<dt_operand *> (kids[i]);
	  if (expr *e = dyn_cast <expr *> (op->op))
	    {
	      if (e->ops.length () == 0
		  && (!gimple || !(*e->operation == CONSTRUCTOR)))
		generic_exprs.safe_push (op);
	      else if (e->operation->kind == id_base::FN)
		{
		  if (gimple)
		    fns.safe_push (op);
		  else
		    generic_fns.safe_push (op);
		}
	      else if (e->operation->kind == id_base::PREDICATE)
		preds.safe_push (op);
	      else
		{
		  if (gimple)
		    gimple_exprs.safe_push (op);
		  else
		    generic_exprs.safe_push (op);
		}
	    }
	  else if (op->op->type == operand::OP_PREDICATE)
	    others.safe_push (kids[i]);
	  else
	    gcc_unreachable ();
	}
      else if (kids[i]->type == dt_node::DT_MATCH
	       || kids[i]->type == dt_node::DT_SIMPLIFY)
	others.safe_push (kids[i]);
      else if (kids[i]->type == dt_node::DT_TRUE)
	{
	  /* A DT_TRUE operand serves as a barrier - generate code now
	     for what we have collected sofar.  */
	  gen_kids_1 (f, gimple, gimple_exprs, generic_exprs,
		      fns, generic_fns, preds, others);
	  /* And output the true operand itself.  */
	  kids[i]->gen (f, gimple);
	  gimple_exprs.truncate (0);
	  generic_exprs.truncate (0);
	  fns.truncate (0);
	  generic_fns.truncate (0);
	  preds.truncate (0);
	  others.truncate (0);
	}
      else
	gcc_unreachable ();
    }

  /* Generate code for the remains.  */
  gen_kids_1 (f, gimple, gimple_exprs, generic_exprs,
	      fns, generic_fns, preds, others);
}

/* Generate matching code for the children of the decision tree node.  */

void
dt_node::gen_kids_1 (FILE *f, bool gimple,
		     vec<dt_operand *> gimple_exprs,
		     vec<dt_operand *> generic_exprs,
		     vec<dt_operand *> fns,
		     vec<dt_operand *> generic_fns,
		     vec<dt_operand *> preds,
		     vec<dt_node *> others)
{
  char buf[128];
  char *kid_opname = buf;

  unsigned exprs_len = gimple_exprs.length ();
  unsigned gexprs_len = generic_exprs.length ();
  unsigned fns_len = fns.length ();
  unsigned gfns_len = generic_fns.length ();

  if (exprs_len || fns_len || gexprs_len || gfns_len)
    {
      if (exprs_len)
	gimple_exprs[0]->get_name (kid_opname);
      else if (fns_len)
	fns[0]->get_name (kid_opname);
      else if (gfns_len)
	generic_fns[0]->get_name (kid_opname);
      else
	generic_exprs[0]->get_name (kid_opname);

      fprintf (f, "switch (TREE_CODE (%s))\n"
	       "{\n", kid_opname);
    }

  if (exprs_len || fns_len)
    {
      fprintf (f, "case SSA_NAME:\n");
      fprintf (f, "if (do_valueize (valueize, %s) != NULL_TREE)\n", kid_opname);
      fprintf (f, "{\n");
      fprintf (f, "gimple def_stmt = SSA_NAME_DEF_STMT (%s);\n", kid_opname);

      if (exprs_len)
	{
	  fprintf (f, "if (is_gimple_assign (def_stmt))\n");
	  fprintf (f, "switch (gimple_assign_rhs_code (def_stmt))\n"
		   "{\n");
	  for (unsigned i = 0; i < exprs_len; ++i)
	    {
	      expr *e = as_a <expr *> (gimple_exprs[i]->op);
	      id_base *op = e->operation;
	      if (*op == CONVERT_EXPR || *op == NOP_EXPR)
		fprintf (f, "CASE_CONVERT:\n");
	      else
		fprintf (f, "case %s:\n", op->id);
	      fprintf (f, "{\n");
	      gimple_exprs[i]->gen (f, true);
	      fprintf (f, "break;\n"
		       "}\n");
	    }
	  fprintf (f, "default:;\n"
		   "}\n");
	}

      if (fns_len)
	{
	  if (exprs_len)
	    fprintf (f, "else ");

	  fprintf (f, "if (gimple_call_builtin_p (def_stmt, BUILT_IN_NORMAL))\n"
		   "{\n"
		   "tree fndecl = gimple_call_fndecl (def_stmt);\n"
		   "switch (DECL_FUNCTION_CODE (fndecl))\n"
		   "{\n");

	  for (unsigned i = 0; i < fns_len; ++i)
	    {
	      expr *e = as_a <expr *>(fns[i]->op);
	      fprintf (f, "case %s:\n"
		       "{\n", e->operation->id);
	      fns[i]->gen (f, true);
	      fprintf (f, "break;\n"
		       "}\n");
	    }

	  fprintf (f, "default:;\n"
		   "}\n"
		   "}\n");
	}

      fprintf (f, "}\n"
	       "break;\n");
    }

  for (unsigned i = 0; i < generic_exprs.length (); ++i)
    {
      expr *e = as_a <expr *>(generic_exprs[i]->op);
      id_base *op = e->operation;
      if (*op == CONVERT_EXPR || *op == NOP_EXPR)
	fprintf (f, "CASE_CONVERT:\n");
      else
	fprintf (f, "case %s:\n", op->id);
      fprintf (f, "{\n");
      generic_exprs[i]->gen (f, gimple);
      fprintf (f, "break;\n"
	       "}\n");
    }

  if (gfns_len)
    {
      fprintf (f, "case CALL_EXPR:\n"
	       "{\n"
	       "tree fndecl = get_callee_fndecl (%s);\n"
	       "if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)\n"
	       "switch (DECL_FUNCTION_CODE (fndecl))\n"
	       "{\n", kid_opname);

      for (unsigned j = 0; j < generic_fns.length (); ++j)
	{
	  expr *e = as_a <expr *>(generic_fns[j]->op);
	  gcc_assert (e->operation->kind == id_base::FN);

	  fprintf (f, "case %s:\n"
		   "{\n", e->operation->id);
	  generic_fns[j]->gen (f, false);
	  fprintf (f, "break;\n"
		   "}\n");
	}

      fprintf (f, "default:;\n"
	       "}\n"
	       "break;\n"
	       "}\n");
    }

  /* Close switch (TREE_CODE ()).  */
  if (exprs_len || fns_len || gexprs_len || gfns_len)
    fprintf (f, "default:;\n"
	     "}\n");

  for (unsigned i = 0; i < preds.length (); ++i)
    {
      expr *e = as_a <expr *> (preds[i]->op);
      predicate_id *p = as_a <predicate_id *> (e->operation);
      preds[i]->get_name (kid_opname);
      fprintf (f, "tree %s_pops[%d];\n", kid_opname, p->nargs);
      fprintf (f, "if (%s_%s (%s, %s_pops%s))\n",
	       gimple ? "gimple" : "tree",
	       p->id, kid_opname, kid_opname,
	       gimple ? ", valueize" : "");
      fprintf (f, "{\n");
      for (int j = 0; j < p->nargs; ++j)
	{
	  char child_opname[20];
	  preds[i]->gen_opname (child_opname, j);
	  fprintf (f, "tree %s = %s_pops[%d];\n", child_opname, kid_opname, j);
	}
      preds[i]->gen_kids (f, gimple);
      fprintf (f, "}\n");
    }

  for (unsigned i = 0; i < others.length (); ++i)
    others[i]->gen (f, gimple);
}

/* Generate matching code for the decision tree operand.  */

void
dt_operand::gen (FILE *f, bool gimple)
{
  char opname[20];
  get_name (opname);

  unsigned n_braces = 0;

  if (type == DT_OPERAND)
    switch (op->type)
      {
	case operand::OP_PREDICATE:
	  n_braces = gen_predicate (f, opname, gimple);
	  break;

	case operand::OP_EXPR:
	  if (gimple)
	    n_braces = gen_gimple_expr (f);
	  else
	    n_braces = gen_generic_expr (f, opname);
	  break;

	default:
	  gcc_unreachable ();
      }
  else if (type == DT_TRUE)
    ;
  else if (type == DT_MATCH)
    n_braces = gen_match_op (f, opname);
  else
    gcc_unreachable ();

  gen_kids (f, gimple);

  for (unsigned i = 0; i < n_braces; ++i)
    fprintf (f, "}\n");
}



/* Generate code for the '(if ...)', '(with ..)' and actual transform
   step of a '(simplify ...)' or '(match ...)'.  This handles everything
   that is not part of the decision tree (simplify->match).  */

void
dt_simplify::gen (FILE *f, bool gimple)
{
  fprintf (f, "{\n");
  output_line_directive (f, s->result_location);
  if (s->capture_max >= 0)
    fprintf (f, "tree captures[%u] ATTRIBUTE_UNUSED = {};\n",
	     s->capture_max + 1);

  for (int i = 0; i <= s->capture_max; ++i)
    if (indexes[i])
      {
	char opname[20];
	fprintf (f, "captures[%u] = %s;\n", i, indexes[i]->get_name (opname));
      }

  unsigned n_braces = 0;
  if (s->ifexpr_vec != vNULL)
    {
      for (unsigned i = 0; i < s->ifexpr_vec.length (); ++i)
	{
	  if_or_with &w = s->ifexpr_vec[i];
	  if (w.is_with)
	    {
	      fprintf (f, "{\n");
	      output_line_directive (f, w.location);
	      w.cexpr->gen_transform (f, NULL, true, 1, "type", NULL);
	      n_braces++;
	    }
	  else
	    {
	      output_line_directive (f, w.location);
	      fprintf (f, "if (");
	      if (i == s->ifexpr_vec.length () - 1
		  || s->ifexpr_vec[i+1].is_with)
		w.cexpr->gen_transform (f, NULL, true, 1, "type", NULL);
	      else
		{
		  unsigned j = i;
		  do
		    {
		      if (j != i)
			{
			  fprintf (f, "\n");
			  output_line_directive (f, s->ifexpr_vec[j].location);
			  fprintf (f, "&& ");
			}
		      fprintf (f, "(");
		      s->ifexpr_vec[j].cexpr->gen_transform (f, NULL,
							     true, 1, "type",
							     NULL);
		      fprintf (f, ")");
		      ++j;
		    }
		  while (j < s->ifexpr_vec.length ()
			 && !s->ifexpr_vec[j].is_with);
		  i = j - 1;
		}
	      fprintf (f, ")\n");
	    }
	}
      fprintf (f, "{\n");
      n_braces++;
    }

  /* Analyze captures and perform early-outs on the incoming arguments
     that cover cases we cannot handle.  */
  capture_info cinfo (s);
  expr *e;
  if (!gimple
      && s->result
      && !((e = dyn_cast <expr *> (s->result))
	   && is_a <predicate_id *> (e->operation)))
    {
      for (unsigned i = 0; i < as_a <expr *> (s->match)->ops.length (); ++i)
	if (cinfo.force_no_side_effects & (1 << i))
	  fprintf (f, "if (TREE_SIDE_EFFECTS (op%d)) return NULL_TREE;\n", i);
      for (int i = 0; i <= s->capture_max; ++i)
	if (cinfo.info[i].cse_p)
	  ;
	else if (cinfo.info[i].force_no_side_effects_p
		 && (cinfo.info[i].toplevel_msk
		     & cinfo.force_no_side_effects) == 0)
	  fprintf (f, "if (TREE_SIDE_EFFECTS (captures[%d])) "
		   "return NULL_TREE;\n", i);
	else if ((cinfo.info[i].toplevel_msk
		  & cinfo.force_no_side_effects) != 0)
	  /* Mark capture as having no side-effects if we had to verify
	     that via forced toplevel operand checks.  */
	  cinfo.info[i].force_no_side_effects_p = true;
    }

  fprintf (f, "if (dump_file && (dump_flags & TDF_DETAILS)) "
	   "fprintf (dump_file, \"Applying pattern ");
  output_line_directive (f, s->result_location, true);
  fprintf (f, ", %%s:%%d\\n\", __FILE__, __LINE__);\n");

  operand *result = s->result;
  if (!result)
    {
      /* If there is no result then this is a predicate implementation.  */
      fprintf (f, "return true;\n");
    }
  else if (gimple)
    {
      /* For GIMPLE simply drop NON_LVALUE_EXPR (which only appears
         in outermost position).  */
      if (result->type == operand::OP_EXPR
	  && *as_a <expr *> (result)->operation == NON_LVALUE_EXPR)
	result = as_a <expr *> (result)->ops[0];
      if (result->type == operand::OP_EXPR)
	{
	  expr *e = as_a <expr *> (result);
	  bool is_predicate = is_a <predicate_id *> (e->operation);
	  if (!is_predicate)
	    fprintf (f, "*res_code = %s;\n",
		     *e->operation == CONVERT_EXPR
		     ? "NOP_EXPR" : e->operation->id);
	  for (unsigned j = 0; j < e->ops.length (); ++j)
	    {
	      char dest[32];
	      snprintf (dest, 32, "  res_ops[%d]", j);
	      const char *optype
		= get_operand_type (e->operation,
				    "type", e->expr_type,
				    j == 0
				    ? NULL : "TREE_TYPE (res_ops[0])");
	      /* We need to expand GENERIC conditions we captured from
	         COND_EXPRs.  */
	      bool expand_generic_cond_exprs_p
	        = (!is_predicate
		   /* But avoid doing that if the GENERIC condition is
		      valid - which it is in the first operand of COND_EXPRs
		      and VEC_COND_EXRPs.  */
		   && ((!(*e->operation == COND_EXPR)
			&& !(*e->operation == VEC_COND_EXPR))
		       || j != 0));
	      e->ops[j]->gen_transform (f, dest, true, 1, optype, &cinfo,
					indexes, expand_generic_cond_exprs_p);
	    }

	  /* Re-fold the toplevel result.  It's basically an embedded
	     gimple_build w/o actually building the stmt.  */
	  if (!is_predicate)
	    fprintf (f, "gimple_resimplify%d (seq, res_code, type, "
		     "res_ops, valueize);\n", e->ops.length ());
	}
      else if (result->type == operand::OP_CAPTURE
	       || result->type == operand::OP_C_EXPR)
	{
	  result->gen_transform (f, "res_ops[0]", true, 1, "type",
				 &cinfo, indexes, false);
	  fprintf (f, "*res_code = TREE_CODE (res_ops[0]);\n");
	  if (is_a <capture *> (result)
	      && cinfo.info[as_a <capture *> (result)->where].cond_expr_cond_p)
	    {
	      /* ???  Stupid tcc_comparison GENERIC trees in COND_EXPRs.  Deal
		 with substituting a capture of that.  */
	      fprintf (f, "if (COMPARISON_CLASS_P (res_ops[0]))\n"
		       "  {\n"
		       "    tree tem = res_ops[0];\n"
		       "    res_ops[0] = TREE_OPERAND (tem, 0);\n"
		       "    res_ops[1] = TREE_OPERAND (tem, 1);\n"
		       "  }\n");
	    }
	}
      else
	gcc_unreachable ();
      fprintf (f, "return true;\n");
    }
  else /* GENERIC */
    {
      bool is_predicate = false;
      if (result->type == operand::OP_EXPR)
	{
	  expr *e = as_a <expr *> (result);
	  is_predicate = is_a <predicate_id *> (e->operation);
	  /* Search for captures used multiple times in the result expression
	     and dependent on TREE_SIDE_EFFECTS emit a SAVE_EXPR.  */
	  if (!is_predicate)
	    for (int i = 0; i < s->capture_max + 1; ++i)
	      {
		if (!cinfo.info[i].force_no_side_effects_p
		    && cinfo.info[i].result_use_count > 1)
		  fprintf (f, "  if (TREE_SIDE_EFFECTS (captures[%d]))\n"
			   "    captures[%d] = save_expr (captures[%d]);\n",
			   i, i, i);
	      }
	  for (unsigned j = 0; j < e->ops.length (); ++j)
	    {
	      char dest[32];
	      if (is_predicate)
		snprintf (dest, 32, "res_ops[%d]", j);
	      else
		{
		  fprintf (f, "   tree res_op%d;\n", j);
		  snprintf (dest, 32, "  res_op%d", j);
		}
	      const char *optype
	        = get_operand_type (e->operation,
				    "type", e->expr_type,
				    j == 0
				    ? NULL : "TREE_TYPE (res_op0)");
	      e->ops[j]->gen_transform (f, dest, false, 1, optype,
					&cinfo, indexes);
	    }
	  if (is_predicate)
	    fprintf (f, "return true;\n");
	  else
	    {
	      fprintf (f, "  tree res;\n");
	      /* Re-fold the toplevel result.  Use non_lvalue to
	         build NON_LVALUE_EXPRs so they get properly
		 ignored when in GIMPLE form.  */
	      if (*e->operation == NON_LVALUE_EXPR)
		fprintf (f, "  res = non_lvalue_loc (loc, res_op0);\n");
	      else
		{
		  if (e->operation->kind == id_base::CODE)
		    fprintf (f, "  res = fold_build%d_loc (loc, %s, type",
			     e->ops.length (),
			     *e->operation == CONVERT_EXPR
			     ? "NOP_EXPR" : e->operation->id);
		  else
		    fprintf (f, "  res = build_call_expr_loc "
			     "(loc, builtin_decl_implicit (%s), %d",
			     e->operation->id, e->ops.length());
		  for (unsigned j = 0; j < e->ops.length (); ++j)
		    fprintf (f, ", res_op%d", j);
		  fprintf (f, ");\n");
		}
	    }
	}
      else if (result->type == operand::OP_CAPTURE
	       || result->type == operand::OP_C_EXPR)

	{
	  fprintf (f, "  tree res;\n");
	  s->result->gen_transform (f, " res", false, 1, "type",
				    &cinfo, indexes);
	}
      else
	gcc_unreachable ();
      if (!is_predicate)
	{
	  /* Search for captures not used in the result expression and dependent
	     on TREE_SIDE_EFFECTS emit omit_one_operand.  */
	  for (int i = 0; i < s->capture_max + 1; ++i)
	    {
	      if (!cinfo.info[i].force_no_side_effects_p
		  && !cinfo.info[i].expr_p
		  && cinfo.info[i].result_use_count == 0)
		fprintf (f, "  if (TREE_SIDE_EFFECTS (captures[%d]))\n"
			 "    res = build2_loc (loc, COMPOUND_EXPR, type,"
			 " fold_ignored_result (captures[%d]), res);\n",
			 i, i);
	    }
	  fprintf (f, "  return res;\n");
	}
    }

  for (unsigned i = 0; i < n_braces; ++i)
    fprintf (f, "}\n");

  fprintf (f, "}\n");
}

/* Main entry to generate code for matching GIMPLE IL off the decision
   tree.  */

void
decision_tree::gen_gimple (FILE *f)
{
  for (unsigned n = 1; n <= 3; ++n)
    {
      fprintf (f, "\nstatic bool\n"
	       "gimple_simplify (code_helper *res_code, tree *res_ops,\n"
	       "                 gimple_seq *seq, tree (*valueize)(tree),\n"
	       "                 code_helper code, tree type");
      for (unsigned i = 0; i < n; ++i)
	fprintf (f, ", tree op%d", i);
      fprintf (f, ")\n");
      fprintf (f, "{\n");

      fprintf (f, "switch (code.get_rep())\n"
	       "{\n");
      for (unsigned i = 0; i < root->kids.length (); i++)
	{
	  dt_operand *dop = static_cast<dt_operand *>(root->kids[i]);
	  expr *e = static_cast<expr *>(dop->op);
	  if (e->ops.length () != n)
	    continue;

	  if (*e->operation == CONVERT_EXPR
	      || *e->operation == NOP_EXPR)
	    fprintf (f, "CASE_CONVERT:\n");
	  else
	    fprintf (f, "case %s%s:\n",
		     is_a <fn_id *> (e->operation) ? "-" : "",
		     e->operation->id);
	  fprintf (f, "{\n");
	  dop->gen_kids (f, true);
	  fprintf (f, "break;\n");
	  fprintf (f, "}\n");
	}
      fprintf (f, "default:;\n"
	       "}\n");

      fprintf (f, "return false;\n");
      fprintf (f, "}\n");
    }
}

/* Main entry to generate code for matching GENERIC IL off the decision
   tree.  */

void
decision_tree::gen_generic (FILE *f)
{
  for (unsigned n = 1; n <= 3; ++n)
    {
      fprintf (f, "\ntree\n"
	       "generic_simplify (location_t loc, enum tree_code code, "
	       "tree type ATTRIBUTE_UNUSED");
      for (unsigned i = 0; i < n; ++i)
	fprintf (f, ", tree op%d", i);
      fprintf (f, ")\n");
      fprintf (f, "{\n");

      fprintf (f, "switch (code)\n"
	       "{\n");
      for (unsigned i = 0; i < root->kids.length (); i++)
	{
	  dt_operand *dop = static_cast<dt_operand *>(root->kids[i]);
	  expr *e = static_cast<expr *>(dop->op);
	  if (e->ops.length () != n
	      /* Builtin simplifications are somewhat premature on
	         GENERIC.  The following drops patterns with outermost
		 calls.  It's easy to emit overloads for function code
		 though if necessary.  */
	      || e->operation->kind != id_base::CODE)
	    continue;

	  operator_id *op_id = static_cast <operator_id *> (e->operation);
	  if (op_id->code == NOP_EXPR || op_id->code == CONVERT_EXPR)
	    fprintf (f, "CASE_CONVERT:\n");
	  else
	    fprintf (f, "case %s:\n", e->operation->id);
	  fprintf (f, "{\n");
	  dop->gen_kids (f, false);
	  fprintf (f, "break;\n"
		   "}\n");
	}
      fprintf (f, "default:;\n"
	       "}\n");

      fprintf (f, "return NULL_TREE;\n");
      fprintf (f, "}\n");
    }
}

/* Output code to implement the predicate P from the decision tree DT.  */

void
write_predicate (FILE *f, predicate_id *p, decision_tree &dt, bool gimple)
{
  fprintf (f, "\nbool\n"
	   "%s%s (tree t%s%s)\n"
	   "{\n", gimple ? "gimple_" : "tree_", p->id,
	   p->nargs > 0 ? ", tree *res_ops" : "",
	   gimple ? ", tree (*valueize)(tree)" : "");
  /* Conveniently make 'type' available.  */
  fprintf (f, "tree type = TREE_TYPE (t);\n");

  if (!gimple)
    fprintf (f, "if (TREE_SIDE_EFFECTS (t)) return false;\n");
  dt.root->gen_kids (f, gimple);

  fprintf (f, "return false;\n"
	   "}\n");
}

/* Write the common header for the GIMPLE/GENERIC IL matching routines.  */

static void
write_header (FILE *f, const char *head)
{
  fprintf (f, "/* Generated automatically by the program `genmatch' from\n");
  fprintf (f, "   a IL pattern matching and simplification description.  */\n");

  /* Include the header instead of writing it awkwardly quoted here.  */
  fprintf (f, "\n#include \"%s\"\n", head);
}



/* AST parsing.  */

class parser
{
public:
  parser (cpp_reader *);

private:
  const cpp_token *next ();
  const cpp_token *peek ();
  const cpp_token *peek_ident (const char * = NULL);
  const cpp_token *expect (enum cpp_ttype);
  void eat_token (enum cpp_ttype);
  const char *get_string ();
  const char *get_ident ();
  void eat_ident (const char *);
  const char *get_number ();

  id_base *parse_operation ();
  operand *parse_capture (operand *);
  operand *parse_expr ();
  c_expr *parse_c_expr (cpp_ttype);
  operand *parse_op ();

  void record_operlist (source_location, user_id *);

  void parse_pattern ();
  void push_simplify (vec<simplify *>&, operand *, source_location,
		      operand *, source_location);
  void parse_simplify (source_location, vec<simplify *>&, predicate_id *,
		       expr *);
  void parse_for (source_location);
  void parse_if (source_location);
  void parse_predicates (source_location);
  void parse_operator_list (source_location);

  cpp_reader *r;
  vec<if_or_with> active_ifs;
  vec<vec<user_id *> > active_fors;
  hash_set<user_id *> *oper_lists_set;
  vec<user_id *> oper_lists;

  cid_map_t *capture_ids;

public:
  vec<simplify *> simplifiers;
  vec<predicate_id *> user_predicates;
  bool parsing_match_operand;
};

/* Lexing helpers.  */

/* Read the next non-whitespace token from R.  */

const cpp_token *
parser::next ()
{
  const cpp_token *token;
  do
    {
      token = cpp_get_token (r);
    }
  while (token->type == CPP_PADDING
	 && token->type != CPP_EOF);
  return token;
}

/* Peek at the next non-whitespace token from R.  */

const cpp_token *
parser::peek ()
{
  const cpp_token *token;
  unsigned i = 0;
  do
    {
      token = cpp_peek_token (r, i++);
    }
  while (token->type == CPP_PADDING
	 && token->type != CPP_EOF);
  /* If we peek at EOF this is a fatal error as it leaves the
     cpp_reader in unusable state.  Assume we really wanted a
     token and thus this EOF is unexpected.  */
  if (token->type == CPP_EOF)
    fatal_at (token, "unexpected end of file");
  return token;
}

/* Peek at the next identifier token (or return NULL if the next
   token is not an identifier or equal to ID if supplied).  */

const cpp_token *
parser::peek_ident (const char *id)
{
  const cpp_token *token = peek ();
  if (token->type != CPP_NAME)
    return 0;

  if (id == 0)
    return token;

  const char *t = (const char *) CPP_HASHNODE (token->val.node.node)->ident.str;
  if (strcmp (id, t) == 0)
    return token;

  return 0;
}

/* Read the next token from R and assert it is of type TK.  */

const cpp_token *
parser::expect (enum cpp_ttype tk)
{
  const cpp_token *token = next ();
  if (token->type != tk)
    fatal_at (token, "expected %s, got %s",
	      cpp_type2name (tk, 0), cpp_type2name (token->type, 0));

  return token;
}

/* Consume the next token from R and assert it is of type TK.  */

void
parser::eat_token (enum cpp_ttype tk)
{
  expect (tk);
}

/* Read the next token from R and assert it is of type CPP_STRING and
   return its value.  */

const char *
parser::get_string ()
{
  const cpp_token *token = expect (CPP_STRING);
  return (const char *)token->val.str.text;
}

/* Read the next token from R and assert it is of type CPP_NAME and
   return its value.  */

const char *
parser::get_ident ()
{
  const cpp_token *token = expect (CPP_NAME);
  return (const char *)CPP_HASHNODE (token->val.node.node)->ident.str;
}

/* Eat an identifier token with value S from R.  */

void
parser::eat_ident (const char *s)
{
  const cpp_token *token = peek ();
  const char *t = get_ident ();
  if (strcmp (s, t) != 0)
    fatal_at (token, "expected '%s' got '%s'\n", s, t);
}

/* Read the next token from R and assert it is of type CPP_NUMBER and
   return its value.  */

const char *
parser::get_number ()
{
  const cpp_token *token = expect (CPP_NUMBER);
  return (const char *)token->val.str.text;
}


/* Record an operator-list use for transparent for handling.  */

void
parser::record_operlist (source_location loc, user_id *p)
{
  if (!oper_lists_set->add (p))
    {
      if (!oper_lists.is_empty ()
	  && oper_lists[0]->substitutes.length () != p->substitutes.length ())
	fatal_at (loc, "User-defined operator list does not have the "
		  "same number of entries as others used in the pattern");
      oper_lists.safe_push (p);
    }
}

/* Parse the operator ID, special-casing convert?, convert1? and
   convert2?  */

id_base *
parser::parse_operation ()
{
  const cpp_token *id_tok = peek ();
  const char *id = get_ident ();
  const cpp_token *token = peek ();
  if (strcmp (id, "convert0") == 0)
    fatal_at (id_tok, "use 'convert?' here");
  if (token->type == CPP_QUERY
      && !(token->flags & PREV_WHITE))
    {
      if (strcmp (id, "convert") == 0)
	id = "convert0";
      else if (strcmp  (id, "convert1") == 0)
	;
      else if (strcmp  (id, "convert2") == 0)
	;
      else
	fatal_at (id_tok, "non-convert operator conditionalized");

      if (!parsing_match_operand)
	fatal_at (id_tok, "conditional convert can only be used in "
		  "match expression");
      eat_token (CPP_QUERY);
    }
  else if (strcmp  (id, "convert1") == 0
	   || strcmp  (id, "convert2") == 0)
    fatal_at (id_tok, "expected '?' after conditional operator");
  id_base *op = get_operator (id);
  if (!op)
    fatal_at (id_tok, "unknown operator %s", id);

  user_id *p = dyn_cast<user_id *> (op);
  if (p && p->is_oper_list)
    record_operlist (id_tok->src_loc, p);
  return op;
}

/* Parse a capture.
     capture = '@'<number>  */

struct operand *
parser::parse_capture (operand *op)
{
  eat_token (CPP_ATSIGN);
  const cpp_token *token = peek ();
  const char *id = NULL;
  if (token->type == CPP_NUMBER)
    id = get_number ();
  else if (token->type == CPP_NAME)
    id = get_ident ();
  else
    fatal_at (token, "expected number or identifier");
  unsigned next_id = capture_ids->elements ();
  bool existed;
  unsigned &num = capture_ids->get_or_insert (id, &existed);
  if (!existed)
    num = next_id;
  return new capture (num, op);
}

/* Parse an expression
     expr = '(' <operation>[capture][flag][type] <operand>... ')'  */

struct operand *
parser::parse_expr ()
{
  expr *e = new expr (parse_operation ());
  const cpp_token *token = peek ();
  operand *op;
  bool is_commutative = false;
  const char *expr_type = NULL;

  if (token->type == CPP_COLON
      && !(token->flags & PREV_WHITE))
    {
      eat_token (CPP_COLON);
      token = peek ();
      if (token->type == CPP_NAME
	  && !(token->flags & PREV_WHITE))
	{
	  const char *s = get_ident ();
	  if (s[0] == 'c' && !s[1])
	    {
	      if (!parsing_match_operand)
		fatal_at (token,
			  "flag 'c' can only be used in match expression");
	      is_commutative = true;
	    }
	  else if (s[1] != '\0')
	    {
	      if (parsing_match_operand)
		fatal_at (token, "type can only be used in result expression");
	      expr_type = s;
	    }
	  else
	    fatal_at (token, "flag %s not recognized", s);

	  token = peek ();
	}
      else
	fatal_at (token, "expected flag or type specifying identifier");
    }

  if (token->type == CPP_ATSIGN
      && !(token->flags & PREV_WHITE))
    op = parse_capture (e);
  else
    op = e;
  do
    {
      const cpp_token *token = peek ();
      if (token->type == CPP_CLOSE_PAREN)
	{
	  if (e->operation->nargs != -1
	      && e->operation->nargs != (int) e->ops.length ())
	    fatal_at (token, "'%s' expects %u operands, not %u",
		      e->operation->id, e->operation->nargs, e->ops.length ());
	  if (is_commutative)
	    {
	      if (e->ops.length () == 2)
		e->is_commutative = true;
	      else
		fatal_at (token, "only binary operators or function with "
			  "two arguments can be marked commutative");
	    }
	  e->expr_type = expr_type;
	  return op;
	}
      e->append_op (parse_op ());
    }
  while (1);
}

/* Lex native C code delimited by START recording the preprocessing tokens
   for later processing.
     c_expr = ('{'|'(') <pp token>... ('}'|')')  */

c_expr *
parser::parse_c_expr (cpp_ttype start)
{
  const cpp_token *token;
  cpp_ttype end;
  unsigned opencnt;
  vec<cpp_token> code = vNULL;
  unsigned nr_stmts = 0;
  eat_token (start);
  if (start == CPP_OPEN_PAREN)
    end = CPP_CLOSE_PAREN;
  else if (start == CPP_OPEN_BRACE)
    end = CPP_CLOSE_BRACE;
  else
    gcc_unreachable ();
  opencnt = 1;
  do
    {
      token = next ();

      /* Count brace pairs to find the end of the expr to match.  */
      if (token->type == start)
	opencnt++;
      else if (token->type == end
	       && --opencnt == 0)
	break;

      /* This is a lame way of counting the number of statements.  */
      if (token->type == CPP_SEMICOLON)
	nr_stmts++;

      /* If this is possibly a user-defined identifier mark it used.  */
      if (token->type == CPP_NAME)
	{
	  id_base *idb = get_operator ((const char *)CPP_HASHNODE
				      (token->val.node.node)->ident.str);
	  user_id *p;
	  if (idb && (p = dyn_cast<user_id *> (idb)) && p->is_oper_list)
	    record_operlist (token->src_loc, p);
	}

      /* Record the token.  */
      code.safe_push (*token);
    }
  while (1);
  return new c_expr (r, code, nr_stmts, vNULL, capture_ids);
}

/* Parse an operand which is either an expression, a predicate or
   a standalone capture.
     op = predicate | expr | c_expr | capture  */

struct operand *
parser::parse_op ()
{
  const cpp_token *token = peek ();
  struct operand *op = NULL;
  if (token->type == CPP_OPEN_PAREN)
    {
      eat_token (CPP_OPEN_PAREN);
      op = parse_expr ();
      eat_token (CPP_CLOSE_PAREN);
    }
  else if (token->type == CPP_OPEN_BRACE)
    {
      op = parse_c_expr (CPP_OPEN_BRACE);
    }
  else
    {
      /* Remaining ops are either empty or predicates  */
      if (token->type == CPP_NAME)
	{
	  const char *id = get_ident ();
	  id_base *opr = get_operator (id);
	  if (!opr)
	    fatal_at (token, "expected predicate name");
	  if (operator_id *code = dyn_cast <operator_id *> (opr))
	    {
	      if (code->nargs != 0)
		fatal_at (token, "using an operator with operands as predicate");
	      /* Parse the zero-operand operator "predicates" as
		 expression.  */
	      op = new expr (opr);
	    }
	  else if (user_id *code = dyn_cast <user_id *> (opr))
	    {
	      if (code->nargs != 0)
		fatal_at (token, "using an operator with operands as predicate");
	      /* Parse the zero-operand operator "predicates" as
		 expression.  */
	      op = new expr (opr);
	    }
	  else if (predicate_id *p = dyn_cast <predicate_id *> (opr))
	    op = new predicate (p);
	  else
	    fatal_at (token, "using an unsupported operator as predicate");
	  if (!parsing_match_operand)
	    fatal_at (token, "predicates are only allowed in match expression");
	  token = peek ();
	  if (token->flags & PREV_WHITE)
	    return op;
	}
      else if (token->type != CPP_COLON
	       && token->type != CPP_ATSIGN)
	fatal_at (token, "expected expression or predicate");
      /* optionally followed by a capture and a predicate.  */
      if (token->type == CPP_COLON)
	fatal_at (token, "not implemented: predicate on leaf operand");
      if (token->type == CPP_ATSIGN)
	op = parse_capture (op);
    }

  return op;
}

/* Create a new simplify from the current parsing state and MATCH,
   MATCH_LOC, RESULT and RESULT_LOC and push it to SIMPLIFIERS.  */

void
parser::push_simplify (vec<simplify *>& simplifiers,
		       operand *match, source_location match_loc,
		       operand *result, source_location result_loc)
{
  /* Build and push a temporary for for operator list uses in expressions.  */
  if (!oper_lists.is_empty ())
    active_fors.safe_push (oper_lists);

  simplifiers.safe_push
    (new simplify (match, match_loc, result, result_loc,
		   active_ifs.copy (), active_fors.copy (), capture_ids));

  if (!oper_lists.is_empty ())
    active_fors.pop ();
}

/* Parse
     simplify = 'simplify' <expr> <result-op>
   or
     match = 'match' <ident> <expr> [<result-op>]
   with
     <result-op> = <op> | <if> | <with>
     <if> = '(' 'if' '(' <c-expr> ')' <result-op> ')'
     <with> = '(' 'with' '{' <c-expr> '}' <result-op> ')'
   and fill SIMPLIFIERS with the results.  */

void
parser::parse_simplify (source_location match_location,
			vec<simplify *>& simplifiers, predicate_id *matcher,
			expr *result)
{
  /* Reset the capture map.  */
  if (!capture_ids)
    capture_ids = new cid_map_t;
  /* Reset oper_lists and set.  */
  hash_set <user_id *> olist;
  oper_lists_set = &olist;
  oper_lists = vNULL;

  const cpp_token *loc = peek ();
  parsing_match_operand = true;
  struct operand *match = parse_op ();
  parsing_match_operand = false;
  if (match->type == operand::OP_CAPTURE && !matcher)
    fatal_at (loc, "outermost expression cannot be captured");
  if (match->type == operand::OP_EXPR
      && is_a <predicate_id *> (as_a <expr *> (match)->operation))
    fatal_at (loc, "outermost expression cannot be a predicate");

  const cpp_token *token = peek ();

  /* If this if is immediately closed then it is part of a predicate
     definition.  Push it.  */
  if (token->type == CPP_CLOSE_PAREN)
    {
      if (!matcher)
	fatal_at (token, "expected transform expression");
      push_simplify (simplifiers, match, match_location,
		     result, token->src_loc);
      return;
    }

  unsigned active_ifs_len = active_ifs.length ();
  while (1)
    {
      if (token->type == CPP_OPEN_PAREN)
	{
	  source_location paren_loc = token->src_loc;
	  eat_token (CPP_OPEN_PAREN);
	  if (peek_ident ("if"))
	    {
	      eat_ident ("if");
	      active_ifs.safe_push (if_or_with (parse_c_expr (CPP_OPEN_PAREN),
						token->src_loc, false));
	      /* If this if is immediately closed then it contains a
	         manual matcher or is part of a predicate definition.
		 Push it.  */
	      if (peek ()->type == CPP_CLOSE_PAREN)
		{
		  if (!matcher)
		    fatal_at (token, "manual transform not implemented");
		  push_simplify (simplifiers, match, match_location,
				 result, paren_loc);
		}
	    }
	  else if (peek_ident ("with"))
	    {
	      eat_ident ("with");
	      /* Parse (with c-expr expr) as (if-with (true) expr).  */
	      c_expr *e = parse_c_expr (CPP_OPEN_BRACE);
	      e->nr_stmts = 0;
	      active_ifs.safe_push (if_or_with (e, token->src_loc, true));
	    }
	  else
	    {
	      operand *op = result;
	      if (!matcher)
		op = parse_expr ();
	      push_simplify (simplifiers, match, match_location,
			     op, token->src_loc);
	      eat_token (CPP_CLOSE_PAREN);
	      /* A "default" result closes the enclosing scope.  */
	      if (active_ifs.length () > active_ifs_len)
		{
		  eat_token (CPP_CLOSE_PAREN);
		  active_ifs.pop ();
		}
	      else
		return;
	    }
	}
      else if (token->type == CPP_CLOSE_PAREN)
	{
	  /* Close a scope if requested.  */
	  if (active_ifs.length () > active_ifs_len)
	    {
	      eat_token (CPP_CLOSE_PAREN);
	      active_ifs.pop ();
	      token = peek ();
	    }
	  else
	    return;
	}
      else
	{
	  if (matcher)
	    fatal_at (token, "expected match operand expression");
	  push_simplify (simplifiers, match, match_location,
			 matcher ? result : parse_op (), token->src_loc);
	  /* A "default" result closes the enclosing scope.  */
	  if (active_ifs.length () > active_ifs_len)
	    {
	      eat_token (CPP_CLOSE_PAREN);
	      active_ifs.pop ();
	    }
	  else
	    return;
	}
      token = peek ();
    }
}

/* Parsing of the outer control structures.  */

/* Parse a for expression
     for = '(' 'for' <subst>... <pattern> ')'
     subst = <ident> '(' <ident>... ')'  */

void
parser::parse_for (source_location)
{
  auto_vec<const cpp_token *> user_id_tokens;
  vec<user_id *> user_ids = vNULL;
  const cpp_token *token;
  unsigned min_n_opers = 0, max_n_opers = 0;

  while (1)
    {
      token = peek ();
      if (token->type != CPP_NAME)
	break;

      /* Insert the user defined operators into the operator hash.  */
      const char *id = get_ident ();
      if (get_operator (id) != NULL)
	fatal_at (token, "operator already defined");
      user_id *op = new user_id (id);
      id_base **slot = operators->find_slot_with_hash (op, op->hashval, INSERT);
      *slot = op;
      user_ids.safe_push (op);
      user_id_tokens.safe_push (token);

      eat_token (CPP_OPEN_PAREN);

      int arity = -1;
      while ((token = peek_ident ()) != 0)
	{
	  const char *oper = get_ident ();
	  id_base *idb = get_operator (oper);
	  if (idb == NULL)
	    fatal_at (token, "no such operator '%s'", oper);
	  if (*idb == CONVERT0 || *idb == CONVERT1 || *idb == CONVERT2)
	    fatal_at (token, "conditional operators cannot be used inside for");

	  if (arity == -1)
	    arity = idb->nargs;
	  else if (idb->nargs == -1)
	    ;
	  else if (idb->nargs != arity)
	    fatal_at (token, "operator '%s' with arity %d does not match "
		      "others with arity %d", oper, idb->nargs, arity);

	  user_id *p = dyn_cast<user_id *> (idb);
	  if (p && p->is_oper_list)
	    op->substitutes.safe_splice (p->substitutes);
	  else 
	    op->substitutes.safe_push (idb);
	}
      op->nargs = arity;
      token = expect (CPP_CLOSE_PAREN);

      unsigned nsubstitutes = op->substitutes.length ();
      if (nsubstitutes == 0)
	fatal_at (token, "A user-defined operator must have at least "
		  "one substitution");
      if (max_n_opers == 0)
	{
	  min_n_opers = nsubstitutes;
	  max_n_opers = nsubstitutes;
	}
      else
	{
	  if (nsubstitutes % min_n_opers != 0
	      && min_n_opers % nsubstitutes != 0)
	    fatal_at (token, "All user-defined identifiers must have a "
		      "multiple number of operator substitutions of the "
		      "smallest number of substitutions");
	  if (nsubstitutes < min_n_opers)
	    min_n_opers = nsubstitutes;
	  else if (nsubstitutes > max_n_opers)
	    max_n_opers = nsubstitutes;
	}
    }

  unsigned n_ids = user_ids.length ();
  if (n_ids == 0)
    fatal_at (token, "for requires at least one user-defined identifier");

  token = peek ();
  if (token->type == CPP_CLOSE_PAREN)
    fatal_at (token, "no pattern defined in for");

  active_fors.safe_push (user_ids);
  while (1)
    {
      token = peek ();
      if (token->type == CPP_CLOSE_PAREN)
 	break;
      parse_pattern ();
    }
  active_fors.pop ();

  /* Remove user-defined operators from the hash again.  */
  for (unsigned i = 0; i < user_ids.length (); ++i)
    {
      if (!user_ids[i]->used)
	warning_at (user_id_tokens[i],
		    "operator %s defined but not used", user_ids[i]->id);
      operators->remove_elt (user_ids[i]);
    }
}

/* Parse an identifier associated with a list of operators.
     oprs = '(' 'define_operator_list' <ident> <ident>... ')'  */

void
parser::parse_operator_list (source_location)
{
  const cpp_token *token = peek (); 
  const char *id = get_ident ();

  if (get_operator (id) != 0)
    fatal_at (token, "operator %s already defined", id);

  user_id *op = new user_id (id, true);
  int arity = -1;
  
  while ((token = peek_ident ()) != 0)
    {
      token = peek (); 
      const char *oper = get_ident ();
      id_base *idb = get_operator (oper);
      
      if (idb == 0)
	fatal_at (token, "no such operator '%s'", oper);

      if (arity == -1)
	arity = idb->nargs;
      else if (idb->nargs == -1)
	;
      else if (arity != idb->nargs)
	fatal_at (token, "operator '%s' with arity %d does not match "
			 "others with arity %d", oper, idb->nargs, arity);

      /* We allow composition of multiple operator lists.  */
      if (user_id *p = dyn_cast<user_id *> (idb))
	op->substitutes.safe_splice (p->substitutes);
      else
	op->substitutes.safe_push (idb);
    }

  if (op->substitutes.length () == 0)
    fatal_at (token, "operator-list cannot be empty");

  op->nargs = arity;
  id_base **slot = operators->find_slot_with_hash (op, op->hashval, INSERT);
  *slot = op;
}

/* Parse an outer if expression.
     if = '(' 'if' '(' <c-expr> ')' <pattern> ')'  */

void
parser::parse_if (source_location loc)
{
  operand *ifexpr = parse_c_expr (CPP_OPEN_PAREN);

  const cpp_token *token = peek ();
  if (token->type == CPP_CLOSE_PAREN)
    fatal_at (token, "no pattern defined in if");

  active_ifs.safe_push (if_or_with (ifexpr, loc, false));
  while (1)
    {
      const cpp_token *token = peek ();
      if (token->type == CPP_CLOSE_PAREN)
	break;

      parse_pattern ();
    }
  active_ifs.pop ();
}

/* Parse a list of predefined predicate identifiers.
     preds = '(' 'define_predicates' <ident>... ')'  */

void
parser::parse_predicates (source_location)
{
  do
    {
      const cpp_token *token = peek ();
      if (token->type != CPP_NAME)
	break;

      add_predicate (get_ident ());
    }
  while (1);
}

/* Parse outer control structures.
     pattern = <preds>|<for>|<if>|<simplify>|<match>  */

void
parser::parse_pattern ()
{
  /* All clauses start with '('.  */
  eat_token (CPP_OPEN_PAREN);
  const cpp_token *token = peek ();
  const char *id = get_ident ();
  if (strcmp (id, "simplify") == 0)
    {
      parse_simplify (token->src_loc, simplifiers, NULL, NULL);
      capture_ids = NULL;
    }
  else if (strcmp (id, "match") == 0)
    {
      bool with_args = false;
      if (peek ()->type == CPP_OPEN_PAREN)
	{
	  eat_token (CPP_OPEN_PAREN);
	  with_args = true;
	}
      const char *name = get_ident ();
      id_base *id = get_operator (name);
      predicate_id *p;
      if (!id)
	{
	  p = add_predicate (name);
	  user_predicates.safe_push (p);
	}
      else if ((p = dyn_cast <predicate_id *> (id)))
	;
      else
	fatal_at (token, "cannot add a match to a non-predicate ID");
      /* Parse (match <id> <arg>... (match-expr)) here.  */
      expr *e = NULL;
      if (with_args)
	{
	  capture_ids = new cid_map_t;
	  e = new expr (p);
	  while (peek ()->type == CPP_ATSIGN)
	    e->append_op (parse_capture (NULL));
	  eat_token (CPP_CLOSE_PAREN);
	}
      if (p->nargs != -1
	  && ((e && e->ops.length () != (unsigned)p->nargs)
	      || (!e && p->nargs != 0)))
	fatal_at (token, "non-matching number of match operands");
      p->nargs = e ? e->ops.length () : 0;
      parse_simplify (token->src_loc, p->matchers, p, e);
      capture_ids = NULL;
    }
  else if (strcmp (id, "for") == 0)
    parse_for (token->src_loc);
  else if (strcmp (id, "if") == 0)
    parse_if (token->src_loc);
  else if (strcmp (id, "define_predicates") == 0)
    {
      if (active_ifs.length () > 0
	  || active_fors.length () > 0)
	fatal_at (token, "define_predicates inside if or for is not supported");
      parse_predicates (token->src_loc);
    }
  else if (strcmp (id, "define_operator_list") == 0)
    {
      if (active_ifs.length () > 0
	  || active_fors.length () > 0)
	fatal_at (token, "operator-list inside if or for is not supported");
      parse_operator_list (token->src_loc);
    }
  else
    fatal_at (token, "expected %s'simplify', 'match', 'for' or 'if'",
	      active_ifs.length () == 0 && active_fors.length () == 0
	      ? "'define_predicates', " : "");

  eat_token (CPP_CLOSE_PAREN);
}

/* Main entry of the parser.  Repeatedly parse outer control structures.  */

parser::parser (cpp_reader *r_)
{
  r = r_;
  active_ifs = vNULL;
  active_fors = vNULL;
  simplifiers = vNULL;
  oper_lists_set = NULL;
  oper_lists = vNULL;
  capture_ids = NULL;
  user_predicates = vNULL;
  parsing_match_operand = false;

  const cpp_token *token = next ();
  while (token->type != CPP_EOF)
    {
      _cpp_backup_tokens (r, 1);
      parse_pattern ();
      token = next ();
    }
}


/* Helper for the linemap code.  */

static size_t
round_alloc_size (size_t s)
{
  return s;
}


/* The genmatch generator progam.  It reads from a pattern description
   and outputs GIMPLE or GENERIC IL matching and simplification routines.  */

int
main (int argc, char **argv)
{
  cpp_reader *r;

  progname = "genmatch";

  if (argc < 2)
    return 1;

  bool gimple = true;
  bool verbose = false;
  char *input = argv[argc-1];
  for (int i = 1; i < argc - 1; ++i)
    {
      if (strcmp (argv[i], "--gimple") == 0)
	gimple = true;
      else if (strcmp (argv[i], "--generic") == 0)
	gimple = false;
      else if (strcmp (argv[i], "-v") == 0)
	verbose = true;
      else
	{
	  fprintf (stderr, "Usage: genmatch "
		   "[--gimple] [--generic] [-v] input\n");
	  return 1;
	}
    }

  line_table = XCNEW (struct line_maps);
  linemap_init (line_table, 0);
  line_table->reallocator = xrealloc;
  line_table->round_alloc_size = round_alloc_size;

  r = cpp_create_reader (CLK_GNUC99, NULL, line_table);
  cpp_callbacks *cb = cpp_get_callbacks (r);
  cb->error = error_cb;

  if (!cpp_read_main_file (r, input))
    return 1;
  cpp_define (r, gimple ? "GIMPLE=1": "GENERIC=1");
  cpp_define (r, gimple ? "GENERIC=0": "GIMPLE=0");

  /* Pre-seed operators.  */
  operators = new hash_table<id_base> (1024);
#define DEFTREECODE(SYM, STRING, TYPE, NARGS) \
  add_operator (SYM, # SYM, # TYPE, NARGS);
#define END_OF_BASE_TREE_CODES
#include "tree.def"
add_operator (CONVERT0, "CONVERT0", "tcc_unary", 1);
add_operator (CONVERT1, "CONVERT1", "tcc_unary", 1);
add_operator (CONVERT2, "CONVERT2", "tcc_unary", 1);
#undef END_OF_BASE_TREE_CODES
#undef DEFTREECODE

  /* Pre-seed builtin functions.
     ???  Cannot use N (name) as that is targetm.emultls.get_address
     for BUILT_IN_EMUTLS_GET_ADDRESS ... */
#define DEF_BUILTIN(ENUM, N, C, T, LT, B, F, NA, AT, IM, COND) \
  add_builtin (ENUM, # ENUM);
#include "builtins.def"
#undef DEF_BUILTIN

  /* Parse ahead!  */
  parser p (r);

  if (gimple)
    write_header (stdout, "gimple-match-head.c");
  else
    write_header (stdout, "generic-match-head.c");

  /* Go over all predicates defined with patterns and perform
     lowering and code generation.  */
  for (unsigned i = 0; i < p.user_predicates.length (); ++i)
    {
      predicate_id *pred = p.user_predicates[i];
      lower (pred->matchers, gimple);

      if (verbose)
	for (unsigned i = 0; i < pred->matchers.length (); ++i)
	  print_matches (pred->matchers[i]);

      decision_tree dt;
      for (unsigned i = 0; i < pred->matchers.length (); ++i)
	dt.insert (pred->matchers[i], i);

      if (verbose)
	dt.print (stderr);

      write_predicate (stdout, pred, dt, gimple);
    }

  /* Lower the main simplifiers and generate code for them.  */
  lower (p.simplifiers, gimple);

  if (verbose)
    for (unsigned i = 0; i < p.simplifiers.length (); ++i)
      print_matches (p.simplifiers[i]);

  decision_tree dt;
  for (unsigned i = 0; i < p.simplifiers.length (); ++i)
    dt.insert (p.simplifiers[i], i);

  if (verbose)
    dt.print (stderr);

  if (gimple)
    dt.gen_gimple (stdout);
  else
    dt.gen_generic (stdout);

  /* Finalize.  */
  cpp_finish (r, NULL);
  cpp_destroy (r);

  delete operators;

  return 0;
}
