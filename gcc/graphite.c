/* Gimple Represented as Polyhedra.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>.

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

/* This pass converts GIMPLE to GRAPHITE, performs some loop
   transformations and then converts the resulting representation back
   to GIMPLE.  

   An early description of this pass can be found in the GCC Summit'06
   paper "GRAPHITE: Polyhedral Analyses and Optimizations for GCC".
   The wiki page http://gcc.gnu.org/wiki/Graphite contains pointers to
   the related work.  

   One important document to read is CLooG's internal manual:
   http://repo.or.cz/w/cloog-ppl.git?a=blob_plain;f=doc/cloog.texi;hb=HEAD
   that describes the data structure of loops used in this file, and
   the functions that are used for transforming the code.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "toplev.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "pointer-set.h"
#include "gimple.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "graphite.h"

static VEC (scop_p, heap) *current_scops;

/* Debug the list of old induction variables for this SCOP.  */

void
debug_oldivs (scop_p scop)
{
  int i;
  name_tree oldiv;

  fprintf (stderr, "Old IVs:");

  for (i = 0; VEC_iterate (name_tree, SCOP_OLDIVS (scop), i, oldiv); i++)
    {
      fprintf (stderr, "(");
      print_generic_expr (stderr, oldiv->t, 0);
      fprintf (stderr, ", %s, %d)\n", oldiv->name, oldiv->loop->num);
    }
  fprintf (stderr, "\n");
}

/* Debug the loops around basic block GB.  */

void
debug_loop_vec (graphite_bb_p gb)
{
  int i;
  loop_p loop;

  fprintf (stderr, "Loop Vec:");

  for (i = 0; VEC_iterate (loop_p, GBB_LOOPS (gb), i, loop); i++)
    fprintf (stderr, "%d: %d, ", i, loop ? loop->num : -1);

  fprintf (stderr, "\n");
}

/* Push (IV, NAME) on STACK.  */

static void 
loop_iv_stack_push (loop_iv_stack stack, tree iv, const char *name)
{
  name_tree named_iv = XNEW (struct name_tree);

  named_iv->t = iv;
  named_iv->name = name;
  VEC_safe_push (name_tree, heap, *stack, named_iv);
}

/* Pops an element out of STACK.  */

static void
loop_iv_stack_pop (loop_iv_stack stack)
{
  VEC_pop (name_tree, *stack);
}

/* Get the IV at INDEX in STACK.  */

static tree
loop_iv_stack_get_iv (loop_iv_stack stack, int index)
{
  name_tree named_iv = VEC_index (name_tree, *stack, index);

  return named_iv->t;
}

/* Get the IV from its NAME in STACK.  */

static tree
loop_iv_stack_get_iv_from_name (loop_iv_stack stack, const char* name)
{
  int i;
  name_tree iv;

  for (i = 0; VEC_iterate (name_tree, *stack, i, iv); i++)
    if (!strcmp (name, iv->name))
      return iv->t;

  return NULL;
}

/* Prints on stderr the contents of STACK.  */

void
loop_iv_stack_debug (loop_iv_stack stack)
{
  int i;
  name_tree iv;
  bool first = true;

  fprintf (stderr, "(");

  for (i = 0; VEC_iterate (name_tree, *stack, i, iv); i++)
    {
      if (first) 
	first = false;
      else
	fprintf (stderr, " ");
      fprintf (stderr, "%s:", iv->name);
      print_generic_expr (stderr, iv->t, 0);
    }

  fprintf (stderr, ")\n");
}

/* In SCOP, get the induction variable from NAME.  OLD is the original
   loop that contained the definition of NAME.  */

static name_tree
get_old_iv_from_ssa_name (scop_p scop, loop_p old, tree name)
{
  tree var = SSA_NAME_VAR (name);
  int i;
  name_tree oldiv;
  
  for (i = 0; VEC_iterate (name_tree, SCOP_OLDIVS (scop), i, oldiv); i++)
    {
      loop_p current = old;

      while (current)
	{
	  if (var == oldiv->t
	      && oldiv->loop == current)
	    return oldiv;

	  current = loop_outer (current);
	}
    }
  return NULL;

}

/* Returns a new loop_to_cloog_loop_str structure.  */

static inline struct loop_to_cloog_loop_str *
new_loop_to_cloog_loop_str (int loop_num,
                            int loop_position,
                            CloogLoop *cloog_loop)
{
  struct loop_to_cloog_loop_str *result;

  result = XNEW (struct loop_to_cloog_loop_str);
  result->loop_num = loop_num;
  result->cloog_loop = cloog_loop;
  result->loop_position = loop_position;

  return result;
}

/* Hash function for SCOP_LOOP2CLOOG_LOOP hash table.  */

static hashval_t
hash_loop_to_cloog_loop (const void *elt)
{
  return ((const struct loop_to_cloog_loop_str *) elt)->loop_num;
}

/* Equality function for SCOP_LOOP2CLOOG_LOOP hash table.  */

static int
eq_loop_to_cloog_loop (const void *el1, const void *el2)
{
  const struct loop_to_cloog_loop_str *elt1, *elt2;

  elt1 = (const struct loop_to_cloog_loop_str *) el1;
  elt2 = (const struct loop_to_cloog_loop_str *) el2;
  return elt1->loop_num == elt2->loop_num;
}

/* Compares two graphite bbs and returns an integer less than, equal to, or
   greater than zero if the first argument is considered to be respectively
   less than, equal to, or greater than the second. 
   We compare using the lexicographic order of the static schedules.  */

static int 
gbb_compare (const void *p_1, const void *p_2)
{
  const struct graphite_bb *const gbb_1
    = *(const struct graphite_bb *const*) p_1;
  const struct graphite_bb *const gbb_2
    = *(const struct graphite_bb *const*) p_2;

  return lambda_vector_compare (GBB_STATIC_SCHEDULE (gbb_1),
                                gbb_nb_loops (gbb_1) + 1,
                                GBB_STATIC_SCHEDULE (gbb_2),
                                gbb_nb_loops (gbb_2) + 1);
}

/* Sort graphite bbs in SCOP.  */

static void
graphite_sort_gbbs (scop_p scop)
{
  VEC (graphite_bb_p, heap) *bbs = SCOP_BBS (scop);

  qsort (VEC_address (graphite_bb_p, bbs),
         VEC_length (graphite_bb_p, bbs),
         sizeof (graphite_bb_p), gbb_compare);
}

/* Dump conditions of a graphite basic block GBB on FILE.  */

static void
dump_gbb_conditions (FILE *file, graphite_bb_p gbb)
{
  int i;
  gimple stmt;
  VEC (gimple, heap) *conditions = GBB_CONDITIONS (gbb);
  
  if (VEC_empty (gimple, conditions))
    return;

  fprintf (file, "\tbb %d\t: cond = {", GBB_BB (gbb)->index);

  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    print_gimple_stmt (file, stmt, 0, 0);

  fprintf (file, "}\n");
}

/* Converts the graphite scheduling function into a cloog scattering
   matrix.  This scattering matrix is used to limit the possible cloog
   output to valid programs in respect to the scheduling function. 

   SCATTERING_DIMENSIONS specifies the dimensionality of the scattering
   matrix. CLooG 0.14.0 and previous versions require, that all scattering
   functions of one CloogProgram have the same dimensionality, therefore we
   allow to specify it. (Should be removed in future versions)  */

static CloogMatrix *
schedule_to_scattering (graphite_bb_p gb, int scattering_dimensions) 
{
  int i;
  scop_p scop = GBB_SCOP (gb);

  int nb_iterators = gbb_nb_loops (gb);

  /* The cloog scattering matrix consists of these colums:
     1                        col  = Eq/Inq,
     scattering_dimensions    cols = Scattering dimensions,
     nb_iterators             cols = bb's iterators,
     scop_nb_params        cols = Parameters,
     1                        col  = Constant 1.

     Example:

     scattering_dimensions = 5
     max_nb_iterators = 2
     nb_iterators = 1 
     scop_nb_params = 2

     Schedule:
     ? i
     4 5

     Scattering Matrix:
     s1  s2  s3  s4  s5  i   p1  p2  1 
     1   0   0   0   0   0   0   0  -4  = 0
     0   1   0   0   0  -1   0   0   0  = 0
     0   0   1   0   0   0   0   0  -5  = 0  */
  int nb_params = scop_nb_params (scop);
  int nb_cols = 1 + scattering_dimensions + nb_iterators + nb_params + 1;
  int col_const = nb_cols - 1; 
  int col_iter_offset = 1 + scattering_dimensions;

  CloogMatrix *scat = cloog_matrix_alloc (scattering_dimensions, nb_cols);

  gcc_assert (scattering_dimensions >= nb_iterators * 2 + 1);

  /* Initialize the identity matrix.  */
  for (i = 0; i < scattering_dimensions; i++)
    value_set_si (scat->p[i][i + 1], 1);

  /* Textual order outside the first loop */
  value_set_si (scat->p[0][col_const], -GBB_STATIC_SCHEDULE (gb)[0]);

  /* For all surrounding loops.  */
  for (i = 0;  i < nb_iterators; i++)
    {
      int schedule = GBB_STATIC_SCHEDULE (gb)[i + 1];

      /* Iterations of this loop.  */
      value_set_si (scat->p[2 * i + 1][col_iter_offset + i], -1);

      /* Textual order inside this loop.  */
      value_set_si (scat->p[2 * i + 2][col_const], -schedule);
    }

  return scat; 
}

/* Print the schedules of GB to FILE with INDENT white spaces before.
   VERBOSITY determines how verbose the code pretty printers are.  */

void
print_graphite_bb (FILE *file, graphite_bb_p gb, int indent, int verbosity)
{
  CloogMatrix *scattering;
  int i;
  loop_p loop;
  fprintf (file, "\nGBB (\n");

  print_loops_bb (file, GBB_BB (gb), indent+2, verbosity);

  if (GBB_DOMAIN (gb))
    {
      fprintf (file, "       (domain: \n");
      cloog_matrix_print (dump_file, GBB_DOMAIN (gb));
      fprintf (file, "       )\n");
    }

  if (GBB_STATIC_SCHEDULE (gb))
    {
      fprintf (file, "       (static schedule: ");
      print_lambda_vector (file, GBB_STATIC_SCHEDULE (gb),
			   gbb_nb_loops (gb) + 1);
      fprintf (file, "       )\n");
    }

  if (GBB_LOOPS (gb))
    {
      fprintf (file, "       (contained loops: \n");
      for (i = 0; VEC_iterate (loop_p, GBB_LOOPS (gb), i, loop); i++)
	if (loop == NULL)
	  fprintf (file, "       iterator %d   =>  NULL \n", i); 
	else
	  fprintf (file, "       iterator %d   =>  loop %d \n", i,
		   loop->num);
      fprintf (file, "       )\n");
    }

  if (GBB_DATA_REFS (gb))
    dump_data_references (file, GBB_DATA_REFS (gb));

  if (GBB_CONDITIONS (gb))
    {
      fprintf (file, "       (conditions: \n");
      dump_gbb_conditions (dump_file, gb);
      fprintf (file, "       )\n");
    }

  if (GBB_SCOP (gb)
      && GBB_STATIC_SCHEDULE (gb))
    {
      fprintf (file, "       (scattering: \n");
      scattering = schedule_to_scattering (gb, 2 * gbb_nb_loops (gb) + 1);
      cloog_matrix_print (file, scattering);
      cloog_matrix_free (scattering);
      fprintf (file, "       )\n");
    }

  fprintf (file, ")\n");
}

/* Print to STDERR the schedules of GB with VERBOSITY level.  */

void
debug_gbb (graphite_bb_p gb, int verbosity)
{
  print_graphite_bb (stderr, gb, 0, verbosity);
}


/* Print SCOP to FILE.  VERBOSITY determines how verbose the pretty
   printers are.  */

static void
print_scop (FILE *file, scop_p scop, int verbosity)
{
  if (scop == NULL)
    return;

  fprintf (file, "\nSCoP_%d_%d (\n",
	   SCOP_ENTRY (scop)->index, SCOP_EXIT (scop)->index);

  fprintf (file, "       (cloog: \n");
  cloog_program_print (file, SCOP_PROG (scop));
  fprintf (file, "       )\n");

  if (SCOP_BBS (scop))
    {
      graphite_bb_p gb;
      int i;

      for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
	print_graphite_bb (file, gb, 0, verbosity);
    }

  fprintf (file, ")\n");
}

/* Print all the SCOPs to FILE.  VERBOSITY determines how verbose the
   code pretty printers are.  */

static void
print_scops (FILE *file, int verbosity)
{
  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, current_scops, i, scop); i++)
    print_scop (file, scop, verbosity);
}

/* Debug SCOP.  VERBOSITY determines how verbose the code pretty
   printers are. */

void
debug_scop (scop_p scop, int verbosity)
{
  print_scop (stderr, scop, verbosity);
}

/* Debug all SCOPs from CURRENT_SCOPS.  VERBOSITY determines how
   verbose the code pretty printers are.  */

void 
debug_scops (int verbosity)
{
  print_scops (stderr, verbosity);
}

/* Return true when BB is contained in SCOP.  */

static inline bool
bb_in_scop_p (basic_block bb, scop_p scop)
{
  return bitmap_bit_p (SCOP_BBS_B (scop), bb->index);
}

/* Pretty print to FILE the SCOP in DOT format.  */

static void 
dot_scop_1 (FILE *file, scop_p scop)
{
  edge e;
  edge_iterator ei;
  basic_block bb;
  basic_block entry = SCOP_ENTRY (scop);
  basic_block exit = SCOP_EXIT (scop);
    
  fprintf (file, "digraph SCoP_%d_%d {\n", entry->index,
	   exit->index);

  FOR_ALL_BB (bb)
    {
      if (bb == entry)
	fprintf (file, "%d [shape=triangle];\n", bb->index);

      if (bb == exit)
	fprintf (file, "%d [shape=box];\n", bb->index);

      if (bb_in_scop_p (bb, scop)) 
	fprintf (file, "%d [color=red];\n", bb->index);

      FOR_EACH_EDGE (e, ei, bb->succs)
	fprintf (file, "%d -> %d;\n", bb->index, e->dest->index);
    }

  fputs ("}\n\n", file);
}

/* Display SCOP using dotty.  */

void
dot_scop (scop_p scop)
{
  dot_scop_1 (stderr, scop);
}

/* Pretty print all SCoPs in DOT format and mark them with different colors.
   If there are not enough colors, paint later SCoPs gray.
   Special nodes:
   - "*" after the node number: entry of a SCoP,
   - "#" after the node number: exit of a SCoP,
   - "()" entry or exit not part of SCoP.  */

static void
dot_all_scops_1 (FILE *file)
{
  basic_block bb;
  edge e;
  edge_iterator ei;
  scop_p scop;
  const char* color;
  int i;

  /* Disable debugging while printing graph.  */
  int tmp_dump_flags = dump_flags;
  dump_flags = 0;

  fprintf (file, "digraph all {\n");

  FOR_ALL_BB (bb)
    {
      int part_of_scop = false;

      /* Use HTML for every bb label.  So we are able to print bbs
         which are part of two different SCoPs, with two different
         background colors.  */
      fprintf (file, "%d [label=<\n  <TABLE BORDER=\"0\" CELLBORDER=\"1\" ",
                     bb->index);
      fprintf (file, "CELLSPACING=\"0\">\n");

      /* Select color for SCoP.  */
      for (i = 0; VEC_iterate (scop_p, current_scops, i, scop); i++)
	if (bb_in_scop_p (bb, scop)
	    || (SCOP_EXIT (scop) == bb)
	    || (SCOP_ENTRY (scop) == bb))
	  {
	    switch (i % 17)
	      {
	      case 0: /* red */
		color = "#e41a1c";
		break;
	      case 1: /* blue */
		color = "#377eb8";
		break;
	      case 2: /* green */
		color = "#4daf4a";
		break;
	      case 3: /* purple */
		color = "#984ea3";
		break;
	      case 4: /* orange */
		color = "#ff7f00";
		break;
	      case 5: /* yellow */
		color = "#ffff33";
		break;
	      case 6: /* brown */
		color = "#a65628";
		break;
	      case 7: /* rose */
		color = "#f781bf";
		break;
	      case 8:
		color = "#8dd3c7";
		break;
	      case 9:
		color = "#ffffb3";
		break;
	      case 10:
		color = "#bebada";
		break;
	      case 11:
		color = "#fb8072";
		break;
	      case 12:
		color = "#80b1d3";
		break;
	      case 13:
		color = "#fdb462";
		break;
	      case 14:
		color = "#b3de69";
		break;
	      case 15:
		color = "#fccde5";
		break;
	      case 16:
		color = "#bc80bd";
		break;
	      default: /* gray */
		color = "#999999";
	      }

	    fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"%s\">", color);
        
	    if (!bb_in_scop_p (bb, scop))
	      fprintf (file, " ("); 

	    if (bb == SCOP_ENTRY (scop)
		&& bb == SCOP_EXIT (scop))
	      fprintf (file, " %d*# ", bb->index);
	    else if (bb == SCOP_ENTRY (scop))
	      fprintf (file, " %d* ", bb->index);
	    else if (bb == SCOP_EXIT (scop))
	      fprintf (file, " %d# ", bb->index);
	    else
	      fprintf (file, " %d ", bb->index);

	    if (!bb_in_scop_p (bb, scop))
	      fprintf (file, ")");

	    fprintf (file, "</TD></TR>\n");
	    part_of_scop  = true;
	  }

      if (!part_of_scop)
        {
          fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"#ffffff\">");
          fprintf (file, " %d </TD></TR>\n", bb->index);
        }

      fprintf (file, "  </TABLE>>, shape=box, style=\"setlinewidth(0)\"]\n");
    }

  FOR_ALL_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	      fprintf (file, "%d -> %d;\n", bb->index, e->dest->index);
    }

  fputs ("}\n\n", file);

  /* Enable debugging again.  */
  dump_flags = tmp_dump_flags;
}

/* Display all SCoPs using dotty.  */

void
dot_all_scops (void)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/allscops.dot", "w");
  gcc_assert (stream);

  dot_all_scops_1 (stream);
  fclose (stream);

  system ("dotty /tmp/allscops.dot");
#else
  dot_all_scops_1 (stderr);
#endif
}

/* Returns true when LOOP is in SCOP.  */

static inline bool 
loop_in_scop_p (struct loop *loop, scop_p scop)
{
  return (bb_in_scop_p (loop->header, scop)
	  && bb_in_scop_p (loop->latch, scop));
}

/* Returns the outermost loop in SCOP that contains BB.  */

static struct loop *
outermost_loop_in_scop (scop_p scop, basic_block bb)
{
  struct loop *nest;

  nest = bb->loop_father;
  while (loop_outer (nest) && loop_in_scop_p (loop_outer (nest), scop))
    nest = loop_outer (nest);

  return nest;
}

/* Returns the block preceding the entry of SCOP.  */

static basic_block
block_before_scop (scop_p scop)
{
  return SESE_ENTRY (SCOP_REGION (scop))->src;
}

/* Return true when EXPR is an affine function in LOOP with parameters
   instantiated relative to SCOP_ENTRY.  */

static bool
loop_affine_expr (basic_block scop_entry, struct loop *loop, tree expr)
{
  int n = loop->num;
  tree scev = analyze_scalar_evolution (loop, expr);

  scev = instantiate_scev (scop_entry, loop, scev);

  return (evolution_function_is_invariant_p (scev, n)
	  || evolution_function_is_affine_multivariate_p (scev, n));
}

/* Return true if the operand OP is simple.  */

static bool
is_simple_operand (loop_p loop, gimple stmt, tree op) 
{
  /* It is not a simple operand when it is a declaration,  */
  if (DECL_P (op)
      /* or a structure,  */
      || AGGREGATE_TYPE_P (TREE_TYPE (op))
      /* or a memory access that cannot be analyzed by the data
	 reference analysis.  */
      || ((handled_component_p (op) || INDIRECT_REF_P (op))
	  && !stmt_simple_memref_p (loop, stmt, op)))
    return false;

  return true;
}

/* Return true only when STMT is simple enough for being handled by
   Graphite.  This depends on SCOP_ENTRY, as the parametetrs are
   initialized relatively to this basic block.  */

static bool
stmt_simple_for_scop_p (basic_block scop_entry, gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  struct loop *loop = bb->loop_father;

  /* GIMPLE_ASM and GIMPLE_CALL may embed arbitrary side effects.
     Calls have side-effects, except those to const or pure
     functions.  */
  if (gimple_has_volatile_ops (stmt)
      || (gimple_code (stmt) == GIMPLE_CALL
	  && !(gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE)))
      || (gimple_code (stmt) == GIMPLE_ASM))
    return false;

  switch (gimple_code (stmt))
    {
    case GIMPLE_RETURN:
    case GIMPLE_LABEL:
      return true;

    case GIMPLE_COND:
      {
	tree op;
	ssa_op_iter op_iter;
        enum tree_code code = gimple_cond_code (stmt);

        /* We can only handle this kind of conditional expressions.  
           For inequalities like "if (i != 3 * k)" we need unions of
           polyhedrons.  Expressions like  "if (a)" or "if (a == 15)" need
           them for the else branch.  */
        if (!(code == LT_EXPR
	      || code == GT_EXPR
              || code == LE_EXPR
	      || code == GE_EXPR))
          return false;

	if (!scop_entry)
	  return false;

	FOR_EACH_SSA_TREE_OPERAND (op, stmt, op_iter, SSA_OP_ALL_USES)
	  if (!loop_affine_expr (scop_entry, loop, op))
	    return false;

	return true;
      }

    case GIMPLE_ASSIGN:
      {
	enum tree_code code = gimple_assign_rhs_code (stmt);

	switch (get_gimple_rhs_class (code))
	  {
	  case GIMPLE_UNARY_RHS:
	  case GIMPLE_SINGLE_RHS:
	    return (is_simple_operand (loop, stmt, gimple_assign_lhs (stmt))
		    && is_simple_operand (loop, stmt, gimple_assign_rhs1 (stmt)));

	  case GIMPLE_BINARY_RHS:
	    return (is_simple_operand (loop, stmt, gimple_assign_lhs (stmt))
		    && is_simple_operand (loop, stmt, gimple_assign_rhs1 (stmt))
		    && is_simple_operand (loop, stmt, gimple_assign_rhs2 (stmt)));

	  case GIMPLE_INVALID_RHS:
	  default:
	    gcc_unreachable ();
	  }
      }

    case GIMPLE_CALL:
      {
	size_t i;
	size_t n = gimple_call_num_args (stmt);
	tree lhs = gimple_call_lhs (stmt);

	for (i = 0; i < n; i++)
	  {
	    tree arg = gimple_call_arg (stmt, i);

	    if (!(is_simple_operand (loop, stmt, lhs)
		  && is_simple_operand (loop, stmt, arg)))
	      return false;
	  }

	return true;
      }

    default:
      /* These nodes cut a new scope.  */
      return false;
    }

  return false;
}

/* Returns the statement of BB that contains a harmful operation: that
   can be a function call with side effects, the induction variables
   are not linear with respect to SCOP_ENTRY, etc.  The current open
   scop should end before this statement.  */

static gimple
harmful_stmt_in_bb (basic_block scop_entry, basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!stmt_simple_for_scop_p (scop_entry, gsi_stmt (gsi)))
      return gsi_stmt (gsi);

  return NULL;
}

/* Store the GRAPHITE representation of BB.  */

static void
new_graphite_bb (scop_p scop, basic_block bb)
{
  struct graphite_bb *gbb = XNEW (struct graphite_bb);

  bb->aux = gbb;
  GBB_BB (gbb) = bb;
  GBB_SCOP (gbb) = scop;
  GBB_DATA_REFS (gbb) = NULL; 
  GBB_DOMAIN (gbb) = NULL;
  GBB_CONDITIONS (gbb) = NULL;
  GBB_CONDITION_CASES (gbb) = NULL;
  GBB_LOOPS (gbb) = NULL;
  VEC_safe_push (graphite_bb_p, heap, SCOP_BBS (scop), gbb);
  bitmap_set_bit (SCOP_BBS_B (scop), bb->index);
}

/* Frees GBB.  */

static void
free_graphite_bb (struct graphite_bb *gbb)
{
  if (GBB_DOMAIN (gbb))
    cloog_matrix_free (GBB_DOMAIN (gbb));

  free_data_refs (GBB_DATA_REFS (gbb));
  VEC_free (gimple, heap, GBB_CONDITIONS (gbb));
  VEC_free (gimple, heap, GBB_CONDITION_CASES (gbb));
  VEC_free (loop_p, heap, GBB_LOOPS (gbb));

  GBB_BB (gbb)->aux = 0;
  XDELETE (gbb);
}

/* Creates a new scop starting with ENTRY.  */

static scop_p
new_scop (edge entry, edge exit)
{
  scop_p scop = XNEW (struct scop);

  gcc_assert (entry && exit);

  SCOP_REGION (scop) = XNEW (struct sese);
  SESE_ENTRY (SCOP_REGION (scop)) = entry;
  SESE_EXIT (SCOP_REGION (scop)) = exit;
  SCOP_BBS (scop) = VEC_alloc (graphite_bb_p, heap, 3);
  SCOP_OLDIVS (scop) = VEC_alloc (name_tree, heap, 3);
  SCOP_BBS_B (scop) = BITMAP_ALLOC (NULL);
  SCOP_LOOPS (scop) = BITMAP_ALLOC (NULL);
  SCOP_LOOP_NEST (scop) = VEC_alloc (loop_p, heap, 3);
  SCOP_PARAMS (scop) = VEC_alloc (name_tree, heap, 3);
  SCOP_PROG (scop) = cloog_program_malloc ();
  cloog_program_set_names (SCOP_PROG (scop), cloog_names_malloc ());
  SCOP_LOOP2CLOOG_LOOP (scop) = htab_create (10, hash_loop_to_cloog_loop,
					     eq_loop_to_cloog_loop,
					     free);
  return scop;
}

/* Deletes SCOP.  */

static void
free_scop (scop_p scop)
{
  int i;
  name_tree p;
  struct graphite_bb *gb;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    free_graphite_bb (gb);

  VEC_free (graphite_bb_p, heap, SCOP_BBS (scop));
  BITMAP_FREE (SCOP_BBS_B (scop));
  BITMAP_FREE (SCOP_LOOPS (scop));
  VEC_free (loop_p, heap, SCOP_LOOP_NEST (scop));
  VEC_free (name_tree, heap, SCOP_OLDIVS (scop));
  
  for (i = 0; VEC_iterate (name_tree, SCOP_PARAMS (scop), i, p); i++)
    free (p);

  VEC_free (name_tree, heap, SCOP_PARAMS (scop));
  cloog_program_free (SCOP_PROG (scop));
  htab_delete (SCOP_LOOP2CLOOG_LOOP (scop)); 
  XDELETE (SCOP_REGION (scop));
  XDELETE (scop);
}

/* Deletes all scops in SCOPS.  */

static void
free_scops (VEC (scop_p, heap) *scops)
{
  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, scops, i, scop); i++)
    free_scop (scop);

  VEC_free (scop_p, heap, scops);
}

typedef enum gbb_type {
  GBB_UNKNOWN,
  GBB_LOOP_SING_EXIT_HEADER,
  GBB_LOOP_MULT_EXIT_HEADER,
  GBB_LOOP_EXIT,
  GBB_COND_HEADER,
  GBB_SIMPLE,
  GBB_LAST
} gbb_type;

/* Detect the type of BB.  Loop headers are only marked, if they are
   new.  This means their loop_father is different to LAST_LOOP.
   Otherwise they are treated like any other bb and their type can be
   any other type.  */

static gbb_type
get_bb_type (basic_block bb, struct loop *last_loop)
{
  VEC (basic_block, heap) *dom;
  int nb_dom, nb_suc;
  struct loop *loop = bb->loop_father;

  /* Check, if we entry into a new loop. */
  if (loop != last_loop)
    {
      if (single_exit (loop) != NULL)
        return GBB_LOOP_SING_EXIT_HEADER;
      else if (loop->num != 0)
        return GBB_LOOP_MULT_EXIT_HEADER;
      else
	return GBB_COND_HEADER;
    }

  dom = get_dominated_by (CDI_DOMINATORS, bb);
  nb_dom = VEC_length (basic_block, dom);
  VEC_free (basic_block, heap, dom);

  if (nb_dom == 0)
    return GBB_LAST;

  nb_suc = VEC_length (edge, bb->succs);

  if (nb_dom == 1 && nb_suc == 1)
    return GBB_SIMPLE;

  return GBB_COND_HEADER;
}

/* A SCoP detection region, defined using bbs as borders. 
   All control flow touching this region, comes in passing basic_block ENTRY and
   leaves passing basic_block EXIT.  By using bbs instead of edges for the
   borders we are able to represent also regions that do not have a single
   entry or exit edge.
   But as they have a single entry basic_block and a single exit basic_block, we
   are able to generate for every sd_region a single entry and exit edge.

   1   2
    \ /
     3	<- entry
     |
     4
    / \			This region contains: {3, 4, 5, 6, 7, 8}
   5   6
   |   |
   7   8
    \ /
     9	<- exit  */


typedef struct sd_region_p
{
  /* The entry bb dominates all bbs in the sd_region.  It is part of the
     region.  */
  basic_block entry;

  /* The exit bb postdominates all bbs in the sd_region, but is not 
     part of the region.  */
  basic_block exit;
} sd_region;

DEF_VEC_O(sd_region);
DEF_VEC_ALLOC_O(sd_region, heap);


/* Moves the scops from SOURCE to TARGET and clean up SOURCE.  */

static void
move_sd_regions (VEC (sd_region, heap) **source, VEC (sd_region, heap) **target)
{
  sd_region *s;
  int i;

  for (i = 0; VEC_iterate (sd_region, *source, i, s); i++)
    VEC_safe_push (sd_region, heap, *target, s);
  
  VEC_free (sd_region, heap, *source);
}

/* Store information needed by scopdet_* functions.  */

struct scopdet_info
{
  /* Where the last open scop would stop if the current BB is harmful.  */
  basic_block last;

  /* Where the next scop would start if the current BB is harmful.  */
  basic_block next;

  /* The bb or one of its children contains open loop exits.  That means
     loop exit nodes that are not surrounded by a loop dominated by bb.  */ 
  bool exits;

  /* The bb or one of its children contains only structures we can handle.  */ 
  bool difficult;
};


static struct scopdet_info build_scops_1 (basic_block, VEC (sd_region, heap) **,
                                          loop_p);

/* Calculates BB infos. If bb is difficult we add valid SCoPs dominated by BB
   to SCOPS.  TYPE is the gbb_type of BB.  */

static struct scopdet_info 
scopdet_basic_block_info (basic_block bb, VEC (sd_region, heap) **scops,
			  gbb_type type)
{
  struct loop *loop = bb->loop_father;
  struct scopdet_info result;
  gimple stmt;

  /* XXX: ENTRY_BLOCK_PTR could be optimized in later steps.  */
  stmt = harmful_stmt_in_bb (ENTRY_BLOCK_PTR, bb);
  result.difficult = (stmt != NULL);
  result.last = NULL;

  switch (type)
    {
    case GBB_LAST:
      result.next = NULL;
      result.exits = false;
      result.last = bb;
      break;

    case GBB_SIMPLE:
      result.next = single_succ (bb);
      result.exits = false;
      result.last = bb;
      break;

    case GBB_LOOP_SING_EXIT_HEADER:
      {
        VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap,3);
        struct scopdet_info sinfo;

        sinfo = build_scops_1 (bb, &tmp_scops, loop);
	
        result.last = single_exit (bb->loop_father)->src;
        result.next = single_exit (bb->loop_father)->dest;

        /* If we do not dominate result.next, remove it.  It's either
           the EXIT_BLOCK_PTR, or another bb dominates it and will
           call the scop detection for this bb.  */
        if (!dominated_by_p (CDI_DOMINATORS, result.next, bb))
	  result.next = NULL;

	if (result.last->loop_father != loop)
	  result.next = NULL;

        if (TREE_CODE (number_of_latch_executions (loop))
            == SCEV_NOT_KNOWN)
          result.difficult = true;

        if (sinfo.difficult)
          move_sd_regions (&tmp_scops, scops);
        else 
          VEC_free (sd_region, heap, tmp_scops);

        result.exits = false;
        result.difficult |= sinfo.difficult;
        break;
      }

    case GBB_LOOP_MULT_EXIT_HEADER:
      {
        /* XXX: For now we just do not join loops with multiple exits. If the 
           exits lead to the same bb it may be possible to join the loop.  */
        VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap, 3);
        VEC (edge, heap) *exits = get_loop_exit_edges (loop);
        edge e;
        int i;
        build_scops_1 (bb, &tmp_scops, loop);

	/* XXX: Use 'e->src' ot better 'bb'?  */
        for (i = 0; VEC_iterate (edge, exits, i, e); i++)
          if (dominated_by_p (CDI_DOMINATORS, e->dest, e->src)
              && e->src->loop_father == loop)
            build_scops_1 (e->dest, &tmp_scops, e->dest->loop_father);

        result.next = NULL; 
        result.last = NULL;
        result.difficult = true;
        result.exits = false;
        move_sd_regions (&tmp_scops, scops);
        VEC_free (edge, heap, exits);
        break;
      }
    case GBB_COND_HEADER:
      {
	VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap, 3);
	struct scopdet_info sinfo;
	VEC (basic_block, heap) *dominated;
	int i;
	basic_block dom_bb;
	basic_block last_bb = NULL;
	edge e;
	result.exits = false;
 
	/* First check the successors of BB, and check if it is possible to join
	   the different branches.  */
	for (i = 0; VEC_iterate (edge, bb->succs, i, e); i++)
	  {
	    /* Ignore loop exits.  They will be handled after the loop body.  */
	    if (is_loop_exit (loop, e->dest))
	      {
		result.exits = true;
		continue;
	      }

	    /* Do not follow edges that lead to the end of the
	       conditions block.  For example, in

               |   0
	       |  /|\
	       | 1 2 |
	       | | | |
	       | 3 4 |
	       |  \|/
               |   6

	       the edge from 0 => 6.  Only check if all paths lead to
	       the same node 6.  */

	    if (!single_pred_p (e->dest))
	      {
		/* Check, if edge leads directly to the end of this
		   condition.  */
		if (!last_bb)
		  {
		    last_bb = e->dest;
		  }

		if (e->dest != last_bb)
		  result.difficult = true;

		continue;
	      }

	    if (!dominated_by_p (CDI_DOMINATORS, e->dest, bb))
	      {
		result.difficult = true;
		continue;
	      }

	    sinfo = build_scops_1 (e->dest, &tmp_scops, loop);

	    result.exits |= sinfo.exits;
	    result.last = sinfo.last;
	    result.difficult |= sinfo.difficult; 

	    /* Checks, if all branches end at the same point. 
	       If that is true, the condition stays joinable.
	       Have a look at the example above.  */
	    if (sinfo.last && single_succ_p (sinfo.last))
	      {
		basic_block next_tmp = single_succ (sinfo.last);
                  
		if (!last_bb)
		    last_bb = next_tmp;

		if (next_tmp != last_bb)
		  result.difficult = true;
	      }
	    else
	      result.difficult = true;
	  }

	/* If the condition is joinable.  */
	if (!result.exits && !result.difficult)
	  {
	    /* Only return a next pointer if we dominate this pointer.
	       Otherwise it will be handled by the bb dominating it.  */ 
	    if (dominated_by_p (CDI_DOMINATORS, last_bb, bb) && last_bb != bb)
	      result.next = last_bb;
	    else
	      result.next = NULL; 

	    VEC_free (sd_region, heap, tmp_scops);
	    break;
	  }

	/* Scan remaining bbs dominated by BB.  */
	dominated = get_dominated_by (CDI_DOMINATORS, bb);

	for (i = 0; VEC_iterate (basic_block, dominated, i, dom_bb); i++)
	  {
	    /* Ignore loop exits: they will be handled after the loop body.  */
	    if (is_loop_exit (loop, dom_bb))
	      {
		result.exits = true;
		continue;
	      }

	    /* Ignore the bbs processed above.  */
	    if (single_pred_p (dom_bb) && single_pred (dom_bb) == bb)
	      continue;

	    if (loop_depth (loop) > loop_depth (dom_bb->loop_father))
	      sinfo = build_scops_1 (dom_bb, &tmp_scops, loop_outer (loop));
	    else
	      sinfo = build_scops_1 (dom_bb, &tmp_scops, loop);
                                           
                                     
	    result.exits |= sinfo.exits; 
	    result.difficult = true;
	    result.last = NULL;
	  }

	VEC_free (basic_block, heap, dominated);

	result.next = NULL; 
	move_sd_regions (&tmp_scops, scops);

	break;
      }

    default:
      gcc_unreachable ();
    }

  return result;
}

/* Creates the SCoPs and writes entry and exit points for every SCoP.  */

static struct scopdet_info 
build_scops_1 (basic_block current, VEC (sd_region, heap) **scops, loop_p loop)
{

  bool in_scop = false;
  sd_region open_scop;
  struct scopdet_info sinfo;

  /* Initialize result.  */ 
  struct scopdet_info result;
  result.exits = false;
  result.difficult = false;
  result.next = NULL;
  result.last = NULL;
  open_scop.entry = NULL;

  /* Loop over the dominance tree.  If we meet a difficult bb, close
     the current SCoP.  Loop and condition header start a new layer,
     and can only be added if all bbs in deeper layers are simple.  */
  while (current != NULL)
    {
      sinfo = scopdet_basic_block_info (current, scops, get_bb_type (current,
								     loop));

      if (!in_scop && !(sinfo.exits || sinfo.difficult))
        {
	  open_scop.entry = current;
	  open_scop.exit = NULL;
          in_scop = true;
        }
      else if (in_scop && (sinfo.exits || sinfo.difficult))
        {
	  open_scop.exit = current;
          VEC_safe_push (sd_region, heap, *scops, &open_scop); 
          in_scop = false;
        }

      result.difficult |= sinfo.difficult;
      result.exits |= sinfo.exits;

      current = sinfo.next;
    }

  /* Try to close open_scop, if we are still in an open SCoP.  */
  if (in_scop)
    {
      int i;
      edge e;

	for (i = 0; VEC_iterate (edge, sinfo.last->succs, i, e); i++)
	  if (dominated_by_p (CDI_POST_DOMINATORS, sinfo.last, e->dest))
            open_scop.exit = e->dest;

        if (!open_scop.exit && open_scop.entry != sinfo.last)
	  open_scop.exit = sinfo.last;

	if (open_scop.exit)
	  VEC_safe_push (sd_region, heap, *scops, &open_scop);
      
    }

  result.last = sinfo.last;
  return result;
}

/* Checks if a bb is contained in REGION.  */

static bool
bb_in_sd_region (basic_block bb, sd_region *region)
{
  return dominated_by_p (CDI_DOMINATORS, bb, region->entry)
	 && !(dominated_by_p (CDI_DOMINATORS, bb, region->exit)
	      && !dominated_by_p (CDI_DOMINATORS, region->entry,
				  region->exit));
}

/* Returns the single entry edge of REGION, if it does not exits NULL.  */

static edge
find_single_entry_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge entry = NULL;

  FOR_EACH_EDGE (e, ei, region->entry->preds)
    if (!bb_in_sd_region (e->src, region))
      {
	if (entry)
	  {
	    entry = NULL;
	    break;
	  }

	else
	  entry = e;
      }

  return entry;
}

/* Returns the single exit edge of REGION, if it does not exits NULL.  */

static edge
find_single_exit_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge exit = NULL;

  FOR_EACH_EDGE (e, ei, region->exit->preds)
    if (bb_in_sd_region (e->src, region))
      {
	if (exit)
	  {
	    exit = NULL;
	    break;
	  }

	else
	  exit = e;
      }

  return exit;
}

/* Create a single entry edge for REGION.  */

static void
create_single_entry_edge (sd_region *region)
{
  if (find_single_entry_edge (region))
    return;

  /* There are multiple predecessors for bb_3 

  |  1  2
  |  | /
  |  |/
  |  3	<- entry
  |  |\
  |  | |
  |  4 ^
  |  | |
  |  |/
  |  5

  There are two edges (1->3, 2->3), that point from outside into the region,
  and another one (5->3), a loop latch, lead to bb_3.

  We split bb_3.
  
  |  1  2
  |  | /
  |  |/
  |3.0
  |  |\     (3.0 -> 3.1) = single entry edge
  |3.1 |  	<- entry
  |  | |
  |  | |
  |  4 ^ 
  |  | |
  |  |/
  |  5

  If the loop is part of the SCoP, we have to redirect the loop latches.

  |  1  2
  |  | /
  |  |/
  |3.0
  |  |      (3.0 -> 3.1) = entry edge
  |3.1  	<- entry
  |  |\
  |  | |
  |  4 ^
  |  | |
  |  |/
  |  5  */

  if (region->entry->loop_father->header != region->entry
      || dominated_by_p (CDI_DOMINATORS,
			 loop_latch_edge (region->entry->loop_father)->src,
			 region->exit))
    {
      edge forwarder = split_block_after_labels (region->entry);
      region->entry = forwarder->dest;
    }
  else
    /* This case is never executed, as the loop headers seem always to have a
       single edge pointing from outside into the loop.  */
    gcc_unreachable ();
      
#ifdef ENABLE_CHECKING
  gcc_assert (find_single_entry_edge (region));
#endif
}

/* Check if the sd_region, mentioned in EDGE, has no exit bb.  */

static bool
sd_region_without_exit (edge e)
{
  sd_region *r = (sd_region *) e->aux;

  if (r)
    return r->exit == NULL;
  else
    return false;
}

/* Create a single exit edge for REGION.  */

static void
create_single_exit_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge forwarder = NULL;
  basic_block exit;
  
  if (find_single_exit_edge (region))
    return;

  /* We create a forwarder bb (5) for all edges leaving this region
     (3->5, 4->5).  All other edges leading to the same bb, are moved
     to a new bb (6).  If these edges where part of another region (2->5)
     we update the region->exit pointer, of this region.

     To identify which edge belongs to which region we depend on the e->aux
     pointer in every edge.  It points to the region of the edge or to NULL,
     if the edge is not part of any region.

     1 2 3 4   	1->5 no region, 		2->5 region->exit = 5,
      \| |/    	3->5 region->exit = NULL, 	4->5 region->exit = NULL
        5	<- exit

     changes to

     1 2 3 4   	1->6 no region, 			2->6 region->exit = 6,
     | | \/	3->5 no region,				4->5 no region, 
     | |  5
      \| /	5->6 region->exit = 6
	6 

     Now there is only a single exit edge (5->6).  */
  exit = region->exit;
  region->exit = NULL;
  forwarder = make_forwarder_block (exit, &sd_region_without_exit, NULL);
  
  /* Unmark the edges, that are no longer exit edges.  */
  FOR_EACH_EDGE (e, ei, forwarder->src->preds)
    if (e->aux)
      e->aux = NULL;

  /* Mark the new exit edge.  */ 
  single_succ_edge (forwarder->src)->aux = region;

  /* Update the exit bb of all regions, where exit edges lead to
     forwarder->dest.  */
  FOR_EACH_EDGE (e, ei, forwarder->dest->preds)
    if (e->aux)
      ((sd_region *) e->aux)->exit = forwarder->dest;

#ifdef ENABLE_CHECKING
  gcc_assert (find_single_exit_edge (region));
#endif
}

/* Unmark the exit edges of all REGIONS.  
   See comment in "create_single_exit_edge". */

static void
unmark_exit_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;
  edge e;
  edge_iterator ei;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    FOR_EACH_EDGE (e, ei, s->exit->preds)
      e->aux = NULL;
}


/* Mark the exit edges of all REGIONS.  
   See comment in "create_single_exit_edge". */

static void
mark_exit_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;
  edge e;
  edge_iterator ei;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    FOR_EACH_EDGE (e, ei, s->exit->preds)
      if (bb_in_sd_region (e->src, s))
	e->aux = s;
}


/* Create for all scop regions a single entry and a single exit edge.  */

static void
create_sese_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;


  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    create_single_entry_edge (s);

  mark_exit_edges (regions);

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    create_single_exit_edge (s);

  unmark_exit_edges (regions);

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_dominators (CDI_DOMINATORS);
  verify_ssa (false);
#endif
}

/* Create graphite SCoPs from an array of scop detection regions.  */

static void
build_graphite_scops (VEC (sd_region, heap) *scop_regions)
{
  int i;
  sd_region *s;

  for (i = 0; VEC_iterate (sd_region, scop_regions, i, s); i++)
    {
      edge entry = find_single_entry_edge (s); 
      edge exit = find_single_exit_edge (s);
      scop_p scop = new_scop (entry, exit);
      VEC_safe_push (scop_p, heap, current_scops, scop);

      /* Are there overlapping SCoPs?  */
#ifdef ENABLE_CHECKING
	{
	  int j;
	  sd_region *s2;

	  for (j = 0; VEC_iterate (sd_region, scop_regions, j, s2); j++)
	    if (s != s2)
	      gcc_assert (!bb_in_sd_region (s->entry, s2));
	}
#endif
    }
}

/* Find static control parts.  */

static void
build_scops (void)
{
  struct loop *loop = current_loops->tree_root;
  VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap, 3);

  build_scops_1 (single_succ (ENTRY_BLOCK_PTR), &tmp_scops, loop);
  create_sese_edges (tmp_scops);
  build_graphite_scops (tmp_scops);
}

/* Gather the basic blocks belonging to the SCOP.  */

static void
build_scop_bbs (scop_p scop)
{
  basic_block *stack = XNEWVEC (basic_block, n_basic_blocks + 1);
  sbitmap visited = sbitmap_alloc (last_basic_block);
  int sp = 0;

  sbitmap_zero (visited);
  stack[sp++] = SCOP_ENTRY (scop);

  while (sp)
    {
      basic_block bb = stack[--sp];
      int depth = loop_depth (bb->loop_father);
      int num = bb->loop_father->num;
      edge_iterator ei;
      edge e;

      /* Scop's exit is not in the scop.  Exclude also bbs, which are
	 dominated by the SCoP exit.  These are e.g. loop latches.  */
      if (TEST_BIT (visited, bb->index)
	  || dominated_by_p (CDI_DOMINATORS, bb, SCOP_EXIT (scop))
	  /* Every block in the scop is dominated by scop's entry.  */
	  || !dominated_by_p (CDI_DOMINATORS, bb, SCOP_ENTRY (scop)))
	continue;

      new_graphite_bb (scop, bb);
      SET_BIT (visited, bb->index);

      /* First push the blocks that have to be processed last.  Note
	 that this means that the order in which the code is organized
	 below is important: do not reorder the following code.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (! TEST_BIT (visited, e->dest->index)
	    && (int) loop_depth (e->dest->loop_father) < depth)
	  stack[sp++] = e->dest;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (! TEST_BIT (visited, e->dest->index)
	    && (int) loop_depth (e->dest->loop_father) == depth
	    && e->dest->loop_father->num != num)
	  stack[sp++] = e->dest;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (! TEST_BIT (visited, e->dest->index)
	    && (int) loop_depth (e->dest->loop_father) == depth
	    && e->dest->loop_father->num == num
	    && EDGE_COUNT (e->dest->preds) > 1)
	  stack[sp++] = e->dest;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (! TEST_BIT (visited, e->dest->index)
	    && (int) loop_depth (e->dest->loop_father) == depth
	    && e->dest->loop_father->num == num
	    && EDGE_COUNT (e->dest->preds) == 1)
	  stack[sp++] = e->dest;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (! TEST_BIT (visited, e->dest->index)
	    && (int) loop_depth (e->dest->loop_father) > depth)
	  stack[sp++] = e->dest;
    }

  free (stack);
  sbitmap_free (visited);
}


/* Record LOOP as occuring in SCOP.  */

static void
scop_record_loop (scop_p scop, struct loop *loop)
{
  loop_p parent;
  tree induction_var;

  if (bitmap_bit_p (SCOP_LOOPS (scop), loop->num))
    return;

  parent = loop_outer (loop);
  induction_var = find_induction_var_from_exit_cond (loop);

  if (!bb_in_scop_p (parent->latch, scop))
    parent = NULL;

  if (induction_var != NULL_TREE)
    {
      name_tree oldiv = XNEW (struct name_tree);
      oldiv->t = SSA_NAME_VAR (induction_var);
      if (DECL_NAME (oldiv->t))
	oldiv->name = IDENTIFIER_POINTER (DECL_NAME (oldiv->t));
      else
	{
	  int len = 2 + 16;
	  char *n = XNEWVEC (char, len);
	  snprintf (n, len, "D.%u", DECL_UID (oldiv->t));
	  oldiv->name = n;
	}
      oldiv->loop = loop;

      VEC_safe_push (name_tree, heap, SCOP_OLDIVS (scop), oldiv);
    }

  bitmap_set_bit (SCOP_LOOPS (scop), loop->num);
  VEC_safe_push (loop_p, heap, SCOP_LOOP_NEST (scop), loop);
}

/* Build the loop nests contained in SCOP.  */

static void
build_scop_loop_nests (scop_p scop)
{
  unsigned i;
  graphite_bb_p gb;
  struct loop *loop0, *loop1;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      struct loop *loop = gbb_loop (gb);

      /* Only add loops, if they are completely contained in the SCoP.  */
      if (loop->header == GBB_BB (gb)
	  && bb_in_scop_p (loop->latch, scop))
        scop_record_loop (scop, gbb_loop (gb));
    }

  /* Make sure that the loops in the SCOP_LOOP_NEST are ordered.  It
     can be the case that an inner loop is inserted before an outer
     loop.  To avoid this, semi-sort once.  */
  for (i = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), i, loop0); i++)
    {
      if (VEC_length (loop_p, SCOP_LOOP_NEST (scop)) == i + 1)
	break;

      loop1 = VEC_index (loop_p, SCOP_LOOP_NEST (scop), i + 1);
      if (loop0->num > loop1->num)
	{
	  VEC_replace (loop_p, SCOP_LOOP_NEST (scop), i, loop1);
	  VEC_replace (loop_p, SCOP_LOOP_NEST (scop), i + 1, loop0);
	}
    }
}

/* Calculate the number of loops around GB in the current SCOP.  */

static inline int
nb_loops_around_gb (graphite_bb_p gb)
{
  scop_p scop = GBB_SCOP (gb);
  struct loop *l = gbb_loop (gb);
  int d = 0;

  for (; loop_in_scop_p (l, scop); d++, l = loop_outer (l));

  return d;
}

/* Build for BB the static schedule.

   The STATIC_SCHEDULE is defined like this:

   A
   for (i: ...)
     {
       for (j: ...)
         {
           B
           C 
         }

       for (k: ...)
         {
           D
           E 
         }
     }
   F

   Static schedules for A to F:

     DEPTH
     0 1 2 
   A 0
   B 1 0 0
   C 1 0 1
   D 1 1 0
   E 1 1 1 
   F 2
*/

static void
build_scop_canonical_schedules (scop_p scop)
{
  int i, j;
  graphite_bb_p gb;
  int nb = scop_nb_loops (scop) + 1;

  SCOP_STATIC_SCHEDULE (scop) = lambda_vector_new (nb);

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      int offset = nb_loops_around_gb (gb);

      /* After leaving a loop, it is possible that the schedule is not
	 set at zero.  This loop reinitializes components located
	 after OFFSET.  */

      for (j = offset + 1; j < nb; j++)
	if (SCOP_STATIC_SCHEDULE (scop)[j])
	  {
	    memset (&(SCOP_STATIC_SCHEDULE (scop)[j]), 0,
		    sizeof (int) * (nb - j));
	    ++SCOP_STATIC_SCHEDULE (scop)[offset];
	    break;
	  }

      GBB_STATIC_SCHEDULE (gb) = lambda_vector_new (offset + 1);
      lambda_vector_copy (SCOP_STATIC_SCHEDULE (scop), 
			  GBB_STATIC_SCHEDULE (gb), offset + 1);

      ++SCOP_STATIC_SCHEDULE (scop)[offset];
    }
}

/* Build the LOOPS vector for all bbs in SCOP.  */

static void
build_bb_loops (scop_p scop)
{
  graphite_bb_p gb;
  int i;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      loop_p loop;
      int depth; 

      depth = nb_loops_around_gb (gb) - 1; 

      GBB_LOOPS (gb) = VEC_alloc (loop_p, heap, 3);
      VEC_safe_grow_cleared (loop_p, heap, GBB_LOOPS (gb), depth + 1);

      loop = GBB_BB (gb)->loop_father;  

      while (scop_contains_loop (scop, loop))
        {
          VEC_replace (loop_p, GBB_LOOPS (gb), depth, loop);
          loop = loop_outer (loop);
          depth--;
        }
    }
}

/* Get the index for parameter VAR in SCOP.  */

static int
param_index (tree var, scop_p scop)
{
  int i;
  name_tree p;
  name_tree nvar;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  for (i = 0; VEC_iterate (name_tree, SCOP_PARAMS (scop), i, p); i++)
    if (p->t == var)
      return i;

  nvar = XNEW (struct name_tree);
  nvar->t = var;
  nvar->name = NULL;
  VEC_safe_push (name_tree, heap, SCOP_PARAMS (scop), nvar);
  return VEC_length (name_tree, SCOP_PARAMS (scop)) - 1;
}

/* Scan EXPR and translate it to an inequality vector INEQ that will
   be added, or subtracted, in the constraint domain matrix C at row
   R.  K is the number of columns for loop iterators in C. */ 

static void
scan_tree_for_params (scop_p s, tree e, CloogMatrix *c, int r, Value k,
		      bool subtract)
{
  int cst_col, param_col;

  if (e == chrec_dont_know)
    return;

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      {
	tree left = CHREC_LEFT (e);
	tree right = CHREC_RIGHT (e);
	int var = CHREC_VARIABLE (e);

	if (TREE_CODE (right) != INTEGER_CST)
	  return;

	if (c)
	  {
            int loop_col = scop_gimple_loop_depth (s, get_loop (var)) + 1;

            if (subtract)
              value_sub_int (c->p[r][loop_col], c->p[r][loop_col],
                             int_cst_value (right));
            else
              value_add_int (c->p[r][loop_col], c->p[r][loop_col],
                             int_cst_value (right));
	  }

	switch (TREE_CODE (left))
	  {
	  case POLYNOMIAL_CHREC:
	    scan_tree_for_params (s, left, c, r, k, subtract);
            return;

	  case INTEGER_CST:
	    /* Constant part.  */
	    if (c)
	      {
                int v = int_cst_value (left);
                cst_col = c->NbColumns - 1;

                if (v < 0)
                  {
                    v = -v;
                    subtract = subtract ? false : true;
                  }

                if (subtract)
                  value_sub_int (c->p[r][cst_col], c->p[r][cst_col], v);
                else
                  value_add_int (c->p[r][cst_col], c->p[r][cst_col], v);
	      }
	    return;

	  default:
	    scan_tree_for_params (s, left, c, r, k, subtract);
	    return;
	  }
      }
      break;

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	{
	  Value val;

	  gcc_assert (host_integerp (TREE_OPERAND (e, 1), 0));

	  value_init (val);
	  value_set_si (val, int_cst_value (TREE_OPERAND (e, 1)));
	  value_multiply (k, k, val);
	  value_clear (val);
	  scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
	}
      else
	{
	  Value val;

	  gcc_assert (host_integerp (TREE_OPERAND (e, 0), 0));

	  value_init (val);
	  value_set_si (val, int_cst_value (TREE_OPERAND (e, 0)));
	  value_multiply (k, k, val);
	  value_clear (val);
	  scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, subtract);
	}
      break;

    case PLUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, subtract);
      break;

    case MINUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      value_oppose (k, k);
      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, subtract);
      break;

    case NEGATE_EXPR:
      value_oppose (k, k);
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      break;

    case SSA_NAME:
      param_col = param_index (e, s);

      if (c)
	{
          param_col += c->NbColumns - scop_nb_params (s) - 1;

          if (subtract)
	    value_subtract (c->p[r][param_col], c->p[r][param_col], k);
          else
	    value_addto (c->p[r][param_col], c->p[r][param_col], k);
	}
      break;

    case INTEGER_CST:
      if (c)
	{
          int v = int_cst_value (e);
	  cst_col = c->NbColumns - 1;

          if (v < 0)
          {
            v = -v;
            subtract = subtract ? false : true;
          }
                
          if (subtract)
            value_sub_int (c->p[r][cst_col], c->p[r][cst_col], v); 
          else
            value_add_int (c->p[r][cst_col], c->p[r][cst_col], v);
	}
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Data structure for idx_record_params.  */

struct irp_data
{
  struct loop *loop;
  scop_p scop;
};

/* For a data reference with an ARRAY_REF as its BASE, record the
   parameters occurring in IDX.  DTA is passed in as complementary
   information, and is used by the automatic walker function.  This
   function is a callback for for_each_index.  */

static bool
idx_record_params (tree base, tree *idx, void *dta)
{
  struct irp_data *data = (struct irp_data *) dta;

  if (TREE_CODE (base) != ARRAY_REF)
    return true;

  if (TREE_CODE (*idx) == SSA_NAME)
    {
      tree scev;
      scop_p scop = data->scop;
      struct loop *loop = data->loop;
      Value one;

      scev = analyze_scalar_evolution (loop, *idx);
      scev = instantiate_scev (block_before_scop (scop), loop, scev);

      value_init (one);
      value_set_si (one, 1);
      scan_tree_for_params (scop, scev, NULL, 0, one, false);
      value_clear (one);
    }

  return true;
}

/* Find parameters with respect to SCOP in BB. We are looking in memory
   access functions, conditions and loop bounds.  */

static void
find_params_in_bb (scop_p scop, basic_block bb)
{
  int i;
  data_reference_p dr;
  VEC (data_reference_p, heap) *drs;
  gimple_stmt_iterator gsi;
  struct loop *nest = outermost_loop_in_scop (scop, bb);

  /* Find the parameters used in the memory access functions.  */
  drs = VEC_alloc (data_reference_p, heap, 5);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    find_data_references_in_stmt (nest, gsi_stmt (gsi), &drs);

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr); i++)
    {
      struct irp_data irp;

      irp.loop = bb->loop_father;
      irp.scop = scop;
      for_each_index (&dr->ref, idx_record_params, &irp);
      free_data_ref (dr);
    }

  VEC_free (data_reference_p, heap, drs);

  /* Find parameters in conditional statements.  */ 
  gsi = gsi_last_bb (bb);
  if (!gsi_end_p (gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (gimple_code (stmt) == GIMPLE_COND)
        {
          Value one;
          loop_p loop = bb->loop_father;

          tree lhs, rhs;
          
          lhs = gimple_cond_lhs (stmt);
          lhs = analyze_scalar_evolution (loop, lhs);
          lhs = instantiate_scev (block_before_scop (scop), loop, lhs);

          rhs = gimple_cond_rhs (stmt);
          rhs = analyze_scalar_evolution (loop, rhs);
          rhs = instantiate_scev (block_before_scop (scop), loop, rhs);

          value_init (one);
          scan_tree_for_params (scop, lhs, NULL, 0, one, false);
          value_set_si (one, 1);
          scan_tree_for_params (scop, rhs, NULL, 0, one, false);
          value_clear (one);
       }
    }
}

/* Saves in NV the name of variable P->T.  */

static void
save_var_name (char **nv, int i, name_tree p)
{
  const char *name = get_name (SSA_NAME_VAR (p->t));

  if (name)
    {
      int len = strlen (name) + 16;
      nv[i] = XNEWVEC (char, len);
      snprintf (nv[i], len, "%s_%d", name, SSA_NAME_VERSION (p->t));
    }
  else
    {
      nv[i] = XNEWVEC (char, 16);
      snprintf (nv[i], 2 + 16, "T_%d", SSA_NAME_VERSION (p->t));
    }

  p->name = nv[i];
}

/* Return the maximal loop depth in SCOP.  */

static int
scop_max_loop_depth (scop_p scop)
{
  int i;
  graphite_bb_p gbb;
  int max_nb_loops = 0;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gbb); i++) 
    {    
      int nb_loops = gbb_nb_loops (gbb);
      if (max_nb_loops < nb_loops)
        max_nb_loops = nb_loops;
    }    

  return max_nb_loops;
}

/* Initialize Cloog's parameter names from the names used in GIMPLE.
   Initialize Cloog's iterator names, using 'graphite_iterator_%d'
   from 0 to scop_nb_loops (scop).  */

static void
initialize_cloog_names (scop_p scop)
{
  int i, nb_params = VEC_length (name_tree, SCOP_PARAMS (scop));
  char **params = XNEWVEC (char *, nb_params);
  int nb_iterators = scop_max_loop_depth (scop);
  int nb_scattering= cloog_program_nb_scattdims (SCOP_PROG (scop));
  char **iterators = XNEWVEC (char *, nb_iterators * 2);
  char **scattering = XNEWVEC (char *, nb_scattering);
  name_tree p;

  for (i = 0; VEC_iterate (name_tree, SCOP_PARAMS (scop), i, p); i++)
    save_var_name (params, i, p);

  cloog_names_set_nb_parameters (cloog_program_names (SCOP_PROG (scop)),
				 nb_params);
  cloog_names_set_parameters (cloog_program_names (SCOP_PROG (scop)),
			      params);

  for (i = 0; i < nb_iterators; i++)
    {
      int len = 18 + 16;
      iterators[i] = XNEWVEC (char, len);
      snprintf (iterators[i], len, "graphite_iterator_%d", i);
    }

  cloog_names_set_nb_iterators (cloog_program_names (SCOP_PROG (scop)),
				nb_iterators);
  cloog_names_set_iterators (cloog_program_names (SCOP_PROG (scop)),
			     iterators);

  for (i = 0; i < nb_scattering; i++)
    {
      int len = 2 + 16;
      scattering[i] = XNEWVEC (char, len);
      snprintf (scattering[i], len, "s_%d", i);
    }

  cloog_names_set_nb_scattering (cloog_program_names (SCOP_PROG (scop)),
				 nb_scattering);
  cloog_names_set_scattering (cloog_program_names (SCOP_PROG (scop)),
			      scattering);
}

/* Record the parameters used in the SCOP.  A variable is a parameter
   in a scop if it does not vary during the execution of that scop.  */

static void
find_scop_parameters (scop_p scop)
{
  graphite_bb_p gb;
  unsigned i;
  struct loop *loop;
  Value one;

  value_init (one);
  value_set_si (one, 1);

  /* Find the parameters used in the loop bounds.  */
  for (i = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), i, loop); i++)
    {
      tree nb_iters = number_of_latch_executions (loop);

      if (!chrec_contains_symbols (nb_iters))
	continue;

      nb_iters = analyze_scalar_evolution (loop, nb_iters);
      nb_iters = instantiate_scev (block_before_scop (scop), loop, nb_iters);
      scan_tree_for_params (scop, nb_iters, NULL, 0, one, false);
    }

  value_clear (one);

  /* Find the parameters used in data accesses.  */
  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    find_params_in_bb (scop, GBB_BB (gb));
}

/* Build the context constraints for SCOP: constraints and relations
   on parameters.  */

static void
build_scop_context (scop_p scop)
{
  int nb_params = scop_nb_params (scop);
  CloogMatrix *matrix = cloog_matrix_alloc (1, nb_params + 2);

  /* Insert '0 >= 0' in the context matrix, as it is not allowed to be
     empty. */
 
  value_set_si (matrix->p[0][0], 1);

  value_set_si (matrix->p[0][nb_params + 1], 0);

  cloog_program_set_context (SCOP_PROG (scop),
			     cloog_domain_matrix2domain (matrix));
  cloog_matrix_free (matrix);
}

/* Returns a graphite_bb from BB.  */

static inline graphite_bb_p
gbb_from_bb (basic_block bb)
{
  return (graphite_bb_p) bb->aux;
}

/* Add DOMAIN to all the basic blocks in LOOP.  */

static void
add_bb_domains (struct loop *loop, CloogMatrix *domain)
{
  basic_block *bbs = get_loop_body (loop);
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    if (bbs[i]->loop_father == loop)
      {
        graphite_bb_p gbb = gbb_from_bb (bbs[i]);
        GBB_DOMAIN (gbb) = cloog_matrix_copy (domain);
      }

  free (bbs);
}

/* Builds the constraint matrix for LOOP in SCOP.  NB_OUTER_LOOPS is the
   number of loops surrounding LOOP in SCOP.  OUTER_CSTR gives the
   constraints matrix for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              CloogMatrix *outer_cstr, int nb_outer_loops)
{
  int i, j, row;
  CloogMatrix *cstr;

  int nb_rows = outer_cstr->NbRows + 1;
  int nb_cols = outer_cstr->NbColumns + 1;

  /* Last column of CSTR is the column of constants.  */
  int cst_col = nb_cols - 1;

  /* The column for the current loop is just after the columns of
     other outer loops.  */
  int loop_col = nb_outer_loops + 1;

  tree nb_iters = number_of_latch_executions (loop);

  /* When the number of iterations is a constant or a parameter, we
     add a constraint for the upper bound of the loop.  So add a row
     to the constraint matrix before allocating it.  */
  if (TREE_CODE (nb_iters) == INTEGER_CST
      || !chrec_contains_undetermined (nb_iters))
    nb_rows++;

  cstr = cloog_matrix_alloc (nb_rows, nb_cols);

  /* Copy the outer constraints.  */
  for (i = 0; i < outer_cstr->NbRows; i++)
    {
      /* Copy the eq/ineq and loops columns.  */
      for (j = 0; j < loop_col; j++)
        value_assign (cstr->p[i][j], outer_cstr->p[i][j]);

      /* Leave an empty column in CSTR for the current loop, and then
        copy the parameter columns.  */
      for (j = loop_col; j < outer_cstr->NbColumns; j++)
        value_assign (cstr->p[i][j + 1], outer_cstr->p[i][j]);
    }

  /* 0 <= loop_i */
  row = outer_cstr->NbRows;
  value_set_si (cstr->p[row][0], 1);
  value_set_si (cstr->p[row][loop_col], 1);

  /* loop_i <= nb_iters */
  if (TREE_CODE (nb_iters) == INTEGER_CST)
    {
      row++;
      value_set_si (cstr->p[row][0], 1);
      value_set_si (cstr->p[row][loop_col], -1);

      value_set_si (cstr->p[row][cst_col],
		    int_cst_value (nb_iters));
    }
  else if (!chrec_contains_undetermined (nb_iters))
    {
      /* Otherwise nb_iters contains parameters: scan the nb_iters
	 expression and build its matrix representation.  */
      Value one;

      row++;
      value_set_si (cstr->p[row][0], 1);
      value_set_si (cstr->p[row][loop_col], -1);

      nb_iters = analyze_scalar_evolution (loop, nb_iters);
      nb_iters = instantiate_scev (block_before_scop (scop), loop, nb_iters);

      value_init (one);
      value_set_si (one, 1);
      scan_tree_for_params (scop, nb_iters, cstr, row, one, false);
      value_clear (one);
    }
  else
    gcc_unreachable ();

  if (loop->inner && loop_in_scop_p (loop->inner, scop))
    build_loop_iteration_domains (scop, loop->inner, cstr, nb_outer_loops + 1);

  /* Only go to the next loops, if we are not at the outermost layer.  These
     have to be handled seperately, as we can be sure, that the chain at this
     layer will be connected.  */
  if (nb_outer_loops != 0 && loop->next && loop_in_scop_p (loop->next, scop))
    build_loop_iteration_domains (scop, loop->next, outer_cstr, nb_outer_loops);

  add_bb_domains (loop, cstr);

  cloog_matrix_free (cstr);
}

/* Add conditions to the domain of GB.  */

static void
add_conditions_to_domain (graphite_bb_p gb)
{
  unsigned int i,j;
  gimple stmt;
  VEC (gimple, heap) *conditions = GBB_CONDITIONS (gb);
  CloogMatrix *domain = GBB_DOMAIN (gb);
  scop_p scop = GBB_SCOP (gb);

  unsigned nb_rows;
  unsigned nb_cols;
  unsigned nb_new_rows = 0;
  unsigned row;

  if (VEC_empty (gimple, conditions))
    return;

  if (domain)
    {
      nb_rows = domain->NbRows;
      nb_cols = domain->NbColumns;
    }
  else  
    {
      nb_rows = 0;
      nb_cols = scop_nb_params (scop) + 2;
    }

  /* Count number of necessary new rows to add the conditions to the
     domain.  */
  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    {
      switch (gimple_code (stmt))
        {
        case GIMPLE_COND:
          {
            enum tree_code code = gimple_cond_code (stmt);

            switch (code)
              {
              case NE_EXPR:
              case EQ_EXPR:
                /* NE and EQ statements are not supported right know. */
                gcc_unreachable ();
                break;
              case LT_EXPR:
              case GT_EXPR:
              case LE_EXPR:
              case GE_EXPR:
                nb_new_rows++;
                break;
              default:
                gcc_unreachable ();
                break;
              }
          break;
          }
        case SWITCH_EXPR:
          /* Switch statements are not supported right know.  */
          gcc_unreachable ();
          break;

        default:
          gcc_unreachable ();
          break;
        }
    }


  /* Enlarge the matrix.  */ 
  { 
    CloogMatrix *new_domain;
    new_domain = cloog_matrix_alloc (nb_rows + nb_new_rows, nb_cols);

    for (i = 0; i < nb_rows; i++)
      for (j = 0; j < nb_cols; j++)
          value_assign (new_domain->p[i][j], domain->p[i][j]);

    cloog_matrix_free (domain);
    domain = new_domain;
    GBB_DOMAIN (gb) = new_domain;
  }     

  /* Add the conditions to the new enlarged domain matrix.  */
  row = nb_rows;
  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    {
      switch (gimple_code (stmt))
        {
        case GIMPLE_COND:
          {
            Value one;
            enum tree_code code;
            tree left;
            tree right;
            loop_p loop = GBB_BB (gb)->loop_father;

            left = gimple_cond_lhs (stmt);
            right = gimple_cond_rhs (stmt);

            left = analyze_scalar_evolution (loop, left);
            right = analyze_scalar_evolution (loop, right);

            left = instantiate_scev (block_before_scop (scop), loop, left);
            right = instantiate_scev (block_before_scop (scop), loop, right);

            code = gimple_cond_code (stmt);

            /* The conditions for ELSE-branches are inverted.  */
            if (VEC_index (gimple, gb->condition_cases, i) == NULL)
              code = invert_tree_comparison (code, false);

            switch (code)
              {
              case NE_EXPR:
                /* NE statements are not supported right know. */
                gcc_unreachable ();
                break;
              case EQ_EXPR:
                value_set_si (domain->p[row][0], 1);
                value_init (one);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, true);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, false);
                row++;
                value_set_si (domain->p[row][0], 1);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, false);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, true);
                value_clear (one);
                row++;
                break;
              case LT_EXPR:
                value_set_si (domain->p[row][0], 1);
                value_init (one);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, true);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, false);
                value_sub_int (domain->p[row][nb_cols - 1],
                    domain->p[row][nb_cols - 1], 1); 
                value_clear (one);
                row++;
                break;
              case GT_EXPR:
                value_set_si (domain->p[row][0], 1);
                value_init (one);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, false);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, true);
                value_sub_int (domain->p[row][nb_cols - 1],
                    domain->p[row][nb_cols - 1], 1);
                value_clear (one);
                row++;
                break;
              case LE_EXPR:
                value_set_si (domain->p[row][0], 1);
                value_init (one);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, true);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, false);
                value_clear (one);
                row++;
                break;
              case GE_EXPR:
                value_set_si (domain->p[row][0], 1);
                value_init (one);
                value_set_si (one, 1);
                scan_tree_for_params (scop, left, domain, row, one, false);
                value_set_si (one, 1);
                scan_tree_for_params (scop, right, domain, row, one, true);
                value_clear (one);
                row++;
                break;
              default:
                gcc_unreachable ();
                break;
              }
            break;
          }
        case GIMPLE_SWITCH:
          /* Switch statements are not supported right know.  */
          gcc_unreachable ();
          break;

        default:
          gcc_unreachable ();
          break;
        }
    }
}

/* Helper recursive function.  */

static void
build_scop_conditions_1 (VEC (gimple, heap) **conditions,
			 VEC (gimple, heap) **cases, basic_block bb,
			 scop_p scop)
{
  int i, j;
  graphite_bb_p gbb;
  gimple_stmt_iterator gsi;
  basic_block bb_child, bb_iter;
  VEC (basic_block, heap) *dom;
  
  /* Make sure we are in the SCoP.  */
  if (!bb_in_scop_p (bb, scop))
    return;

  /* Record conditions in graphite_bb.  */
  gbb = gbb_from_bb (bb);
  GBB_CONDITIONS (gbb) = VEC_copy (gimple, heap, *conditions);
  GBB_CONDITION_CASES (gbb) = VEC_copy (gimple, heap, *cases);

  add_conditions_to_domain (gbb);

  dom = get_dominated_by (CDI_DOMINATORS, bb);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      VEC (edge, gc) *edges;
      edge e;

      switch (gimple_code (stmt))
	{
	case GIMPLE_COND:
	  edges = bb->succs;
	  for (i = 0; VEC_iterate (edge, edges, i, e); i++)
	    if ((dominated_by_p (CDI_DOMINATORS, e->dest, bb))
		&& VEC_length (edge, e->dest->preds) == 1)
	      {
		/* Remove the scanned block from the dominator successors.  */
		for (j = 0; VEC_iterate (basic_block, dom, j, bb_iter); j++)
		  if (bb_iter == e->dest)
		    {
		      VEC_unordered_remove (basic_block, dom, j);
		      break;
		    }

		/* Recursively scan the then or else part.  */
		if (e->flags & EDGE_TRUE_VALUE)
		  VEC_safe_push (gimple, heap, *cases, stmt);
		else if (e->flags & EDGE_FALSE_VALUE)
		  VEC_safe_push (gimple, heap, *cases, NULL);
		else
		  gcc_unreachable ();

		VEC_safe_push (gimple, heap, *conditions, stmt);
		build_scop_conditions_1 (conditions, cases, e->dest, scop);
		VEC_pop (gimple, *conditions);
		VEC_pop (gimple, *cases);
	      }
	  break;

	case GIMPLE_SWITCH:
	  {
	    unsigned i;
	    gimple_stmt_iterator gsi_search_gimple_label;

	    for (i = 0; i < gimple_switch_num_labels (stmt); ++i)
	      {
		basic_block bb_iter;
		size_t k;
		size_t n_cases = VEC_length (gimple, *conditions);
		unsigned n = gimple_switch_num_labels (stmt);

		bb_child = label_to_block
		  (CASE_LABEL (gimple_switch_label (stmt, i)));

		/* Do not handle multiple values for the same block.  */
		for (k = 0; k < n; k++)
		  if (i != k
		      && label_to_block 
		      (CASE_LABEL (gimple_switch_label (stmt, k))) == bb_child)
		    break;

		if (k != n)
		  continue;

		/* Switch cases with more than one predecessor are not
		   handled.  */
		if (VEC_length (edge, bb_child->preds) != 1)
		  continue;

		/* Recursively scan the corresponding 'case' block.  */

		for (gsi_search_gimple_label = gsi_start_bb (bb_child);
		     !gsi_end_p (gsi_search_gimple_label);
		     gsi_next (&gsi_search_gimple_label))
		  {
		    gimple stmt_gimple_label 
		      = gsi_stmt (gsi_search_gimple_label);

		    if (gimple_code (stmt_gimple_label) == GIMPLE_LABEL)
		      {
			tree t = gimple_label_label (stmt_gimple_label);

			if (t == gimple_switch_label (stmt, i))
			  VEC_replace (gimple, *cases, n_cases,
				       stmt_gimple_label);
			else
			  gcc_unreachable ();
		      }
		  }

		build_scop_conditions_1 (conditions, cases, bb_child, scop);

		/* Remove the scanned block from the dominator successors.  */
		for (j = 0; VEC_iterate (basic_block, dom, j, bb_iter); j++)
		  if (bb_iter == bb_child)
		    {
		      VEC_unordered_remove (basic_block, dom, j);
		      break;
		    }  
	      }

	    VEC_pop (gimple, *conditions);
	    VEC_pop (gimple, *cases);
	    break;
	  }
	default:
	  break;
      }
  }

  /* Scan all immediate dominated successors.  */
  for (i = 0; VEC_iterate (basic_block, dom, i, bb_child); i++)
    build_scop_conditions_1 (conditions, cases, bb_child, scop);

  VEC_free (basic_block, heap, dom);
}

/* Record all 'if' and 'switch' conditions in each gbb of SCOP.  */

static void
build_scop_conditions (scop_p scop)
{
  VEC (gimple, heap) *conditions = NULL;
  VEC (gimple, heap) *cases = NULL;

  build_scop_conditions_1 (&conditions, &cases, SCOP_ENTRY (scop), scop);

  VEC_free (gimple, heap, conditions);
  VEC_free (gimple, heap, cases);
}

/* Build the current domain matrix: the loops belonging to the current
   SCOP, and that vary for the execution of the current basic block.
   Returns false if there is no loop in SCOP.  */

static bool
build_scop_iteration_domain (scop_p scop)
{
  struct loop *loop;
  CloogMatrix *outer_cstr;
  int i;

  /* Build cloog loop for all loops, that are in the uppermost loop layer of
     this SCoP.  */
  for (i = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), i, loop); i++)
    if (!loop_in_scop_p (loop_outer (loop), scop))
      {
        /* The outermost constraints is a matrix that has:
           -first column: eq/ineq boolean
           -last column: a constant
           -scop_nb_params columns for the parameters used in the scop.  */
       outer_cstr = cloog_matrix_alloc (0, scop_nb_params (scop) + 2);
       build_loop_iteration_domains (scop, loop, outer_cstr, 0);
       cloog_matrix_free (outer_cstr);
     }

  return (i != 0);
}

/* Initializes an equation CY of the access matrix using the
   information for a subscript from ACCESS_FUN, relatively to the loop
   indexes from LOOP_NEST and parameter indexes from PARAMS.  NDIM is
   the dimension of the array access, i.e. the number of
   subscripts.  Returns true when the operation succeeds.  */

static bool
build_access_matrix_with_af (tree access_fun, lambda_vector cy,
			     scop_p scop, int ndim)
{
  switch (TREE_CODE (access_fun))
    {
    case POLYNOMIAL_CHREC:
      {
	tree left = CHREC_LEFT (access_fun);
	tree right = CHREC_RIGHT (access_fun);
	int var;

	if (TREE_CODE (right) != INTEGER_CST)
	  return false;
        
	var = loop_iteration_vector_dim (CHREC_VARIABLE (access_fun), scop);
	cy[var] = int_cst_value (right);

	switch (TREE_CODE (left))
	  {
	  case POLYNOMIAL_CHREC:
	    return build_access_matrix_with_af (left, cy, scop, ndim);

	  case INTEGER_CST:
	    cy[ndim - 1] = int_cst_value (left);
	    return true;

	  default:
	    /* FIXME: access_fn can have parameters.  */
	    return false;
	  }
      }
    case INTEGER_CST:
      cy[ndim - 1] = int_cst_value (access_fun);
      return true;

    default:
      /* FIXME: access_fn can have parameters.  */
      return false;
    }
}

/* Initialize the access matrix in the data reference REF with respect
   to the loop nesting LOOP_NEST.  Return true when the operation
   succeeded.  */

static bool
build_access_matrix (data_reference_p ref, graphite_bb_p gb)
{
  int i, ndim = DR_NUM_DIMENSIONS (ref);
  struct access_matrix *am = GGC_NEW (struct access_matrix);

  AM_MATRIX (am) = VEC_alloc (lambda_vector, heap, ndim);
  DR_SCOP (ref) = GBB_SCOP (gb);

  for (i = 0; i < ndim; i++)
    {
      lambda_vector v = lambda_vector_new (ref_nb_loops (ref));
      scop_p scop = GBB_SCOP (gb);
      tree af = DR_ACCESS_FN (ref, i);

      if (!build_access_matrix_with_af (af, v, scop, ref_nb_loops (ref)))
	return false;

      VEC_safe_push (lambda_vector, heap, AM_MATRIX (am), v);
    }

  DR_ACCESS_MATRIX (ref) = am;
  return true;
}

/* Build the access matrices for the data references in the SCOP.  */

static void
build_scop_data_accesses (scop_p scop)
{
  int i;
  graphite_bb_p gb;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      int j;
      gimple_stmt_iterator gsi;
      data_reference_p dr;
      struct loop *nest = outermost_loop_in_scop (scop, GBB_BB (gb));

      /* On each statement of the basic block, gather all the occurences
	 to read/write memory.  */
      GBB_DATA_REFS (gb) = VEC_alloc (data_reference_p, heap, 5);
      for (gsi = gsi_start_bb (GBB_BB (gb)); !gsi_end_p (gsi); gsi_next (&gsi))
	find_data_references_in_stmt (nest, gsi_stmt (gsi),
				      &GBB_DATA_REFS (gb));

      /* FIXME: Construction of access matrix is disabled until some
	 pass, like the data dependence analysis, is using it.  */
      continue;

      /* Construct the access matrix for each data ref, with respect to
	 the loop nest of the current BB in the considered SCOP.  */
      for (j = 0;
	   VEC_iterate (data_reference_p, GBB_DATA_REFS (gb), j, dr);
	   j++)
	{
	  bool res = build_access_matrix (dr, gb);

	  /* FIXME: At this point the DRs should always have an affine
	     form.  For the moment this fails as build_access_matrix
	     does not build matrices with parameters.  */
	  gcc_assert (res);
	}
    }
}

/* Converts a GMP constant value to a tree and returns it.  */

static tree
gmp_cst_to_tree (Value v)
{
  return build_int_cst (integer_type_node, value_get_si (v));
}

/* Returns the tree variable from the name NAME that was given in
   Cloog representation.  All the parameters are stored in PARAMS, and
   all the loop induction variables are stored in IVSTACK.

   FIXME: This is a hack, and Cloog should be fixed to not work with
   variable names represented as "char *string", but with void
   pointers that could be casted back to a tree.  The only problem in
   doing that is that Cloog's pretty printer still assumes that
   variable names are char *strings.  The solution would be to have a
   function pointer for pretty-printing that can be redirected to be
   print_generic_stmt in our case, or fprintf by default.
   ???  Too ugly to live.  */

static tree
clast_name_to_gcc (const char *name, VEC (name_tree, heap) *params, 
		   loop_iv_stack ivstack)
{
  int i;
  name_tree t;
  tree iv;

  for (i = 0; VEC_iterate (name_tree, params, i, t); i++)
    if (!strcmp (name, t->name))
      return t->t;

  iv = loop_iv_stack_get_iv_from_name (ivstack, name);
  if (iv)
    return iv;

  gcc_unreachable ();
}

/* Converts a Cloog AST expression E back to a GCC expression tree.   */

static tree
clast_to_gcc_expression (struct clast_expr *e,
			 VEC (name_tree, heap) *params,
			 loop_iv_stack ivstack)
{
  tree type = integer_type_node;

  gcc_assert (e);

  switch (e->type)
    {
    case expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  {
	    if (value_one_p (t->val))
 	      return clast_name_to_gcc (t->var, params, ivstack);

	    else if (value_mone_p (t->val))
	      return fold_build1 (NEGATE_EXPR, type,
				  clast_name_to_gcc (t->var, params, ivstack));
	    else
	      return fold_build2 (MULT_EXPR, type,
				  gmp_cst_to_tree (t->val),
				  clast_name_to_gcc (t->var, params, ivstack));
	  }
	else
	  return gmp_cst_to_tree (t->val);
      }

    case expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;
        tree left, right;

        switch (r->type)
          {
	  case clast_red_sum:
	    if (r->n == 1)
	      return clast_to_gcc_expression (r->elts[0], params, ivstack);

	    else 
	      {
		gcc_assert (r->n >= 1
			    && r->elts[0]->type == expr_term
			    && r->elts[1]->type == expr_term);

		left = clast_to_gcc_expression (r->elts[0], params, ivstack);
		right = clast_to_gcc_expression (r->elts[1], params, ivstack);
		return fold_build2 (PLUS_EXPR, type, left, right);
	      }

	    break;

	  case clast_red_min:
	    if (r->n == 1)
	      return clast_to_gcc_expression (r->elts[0], params, ivstack);

	    else if (r->n == 2)
	      {
		left = clast_to_gcc_expression (r->elts[0], params, ivstack);
		right = clast_to_gcc_expression (r->elts[1], params, ivstack);
		return fold_build2 (MIN_EXPR, type, left, right);
	      }

	    else
	      gcc_unreachable();

	    break;

	  case clast_red_max:
	    if (r->n == 1)
	      return clast_to_gcc_expression (r->elts[0], params, ivstack);

	    else if (r->n == 2)
	      {
		left = clast_to_gcc_expression (r->elts[0], params, ivstack);
		right = clast_to_gcc_expression (r->elts[1], params, ivstack);
		return fold_build2 (MAX_EXPR, type, left, right);
	      }

	    else
	      gcc_unreachable();

	    break;

	  default:
	    gcc_unreachable ();
          }
        break;
      }

    case expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	struct clast_expr *rhs = (struct clast_expr *) b->RHS;
	tree tl = clast_to_gcc_expression (lhs, params, ivstack);

	/* FIXME: The next statement produces a warning: Cloog assumes
	   that the RHS is a constant, but this is a "void *" pointer
	   that should be casted into a Value, but this cast cannot be
	   done as Value is a GMP type, that is an array.  Cloog must
	   be fixed for removing this warning.  */
	tree tr = gmp_cst_to_tree (rhs);

	switch (b->type)
	  {
	  case clast_bin_fdiv:
	    return fold_build2 (FLOOR_DIV_EXPR, type, tl, tr);

	  case clast_bin_cdiv:
	    return fold_build2 (CEIL_DIV_EXPR, type, tl, tr);

	  case clast_bin_div:
	    return fold_build2 (EXACT_DIV_EXPR, type, tl, tr);

	  case clast_bin_mod:
	    return fold_build2 (TRUNC_MOD_EXPR, type, tl, tr);

	  default:
	    gcc_unreachable ();
	  }
      }

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Translates a clast equation CLEQ to a tree.  */

static tree
graphite_translate_clast_equation (scop_p scop,
				   struct clast_equation *cleq,
				   loop_iv_stack ivstack)
{
  enum tree_code comp;
  tree lhs = clast_to_gcc_expression (cleq->LHS, SCOP_PARAMS (scop), ivstack);
  tree rhs = clast_to_gcc_expression (cleq->RHS, SCOP_PARAMS (scop), ivstack);

  if (cleq->sign == 0)
    comp = EQ_EXPR;

  else if (cleq->sign > 0)
    comp = GE_EXPR;

  else
    comp = LE_EXPR;

  return fold_build2 (comp, integer_type_node, lhs, rhs);
}

/* Creates the test for the condition in STMT.  */

static tree
graphite_create_guard_cond_expr (scop_p scop, struct clast_guard *stmt, 
				 loop_iv_stack ivstack)
{
  tree cond = NULL;
  int i;

  for (i = 0; i < stmt->n; i++)
    {
      tree eq = graphite_translate_clast_equation (scop, &stmt->eq[i], ivstack);

      if (cond)
	cond = fold_build2 (TRUTH_AND_EXPR, integer_type_node, cond, eq);
      else
	cond = eq;
    }

  return cond;
}

/* Creates a new if region corresponding to Cloog's guard.  */

static edge 
graphite_create_new_guard (scop_p scop, edge entry_edge,
			   struct clast_guard *stmt, 
			   loop_iv_stack ivstack)
{
  tree cond_expr = graphite_create_guard_cond_expr (scop, stmt, ivstack);
  edge exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);
  return exit_edge;
}


/* Creates a new LOOP corresponding to Cloog's STMT.  Inserts an induction 
   variable for the new LOOP.  New LOOP is attached to CFG starting at
   ENTRY_EDGE.  LOOP is inserted into the loop tree and becomes the child
   loop of the OUTER_LOOP.  */

static struct loop *
graphite_create_new_loop (scop_p scop, edge entry_edge,
			  struct clast_for *stmt, loop_iv_stack ivstack,
			  loop_p outer)
{
  struct loop *loop;
  tree ivvar;
  tree stride, lowb, upb;
  tree iv_before;

  gcc_assert (stmt->LB
	      && stmt->UB);

  stride = gmp_cst_to_tree (stmt->stride);
  lowb = clast_to_gcc_expression (stmt->LB, SCOP_PARAMS (scop), ivstack);
  ivvar = create_tmp_var (integer_type_node, "graphiteIV");
  add_referenced_var (ivvar);

  upb = clast_to_gcc_expression (stmt->UB, SCOP_PARAMS (scop), ivstack);
  loop = create_empty_loop_on_edge (entry_edge, lowb, stride, upb, ivvar,
				    &iv_before, outer ? outer
				    : entry_edge->src->loop_father);

  loop_iv_stack_push (ivstack, iv_before, stmt->iterator);

  return loop;
}

/* Remove all the edges from EDGES except the edge KEEP.  */

static void
remove_all_edges_1 (VEC (edge, gc) *edges, edge keep)
{
  edge e;
  edge_iterator ei;

  for (ei = ei_start (edges); (e = ei_safe_edge (ei)); )
    {
      if (e != keep)
	{
	  remove_edge (e);
	  e = ei_safe_edge (ei);
	}
      else
	ei_next (&ei);
    }
}

/* Remove all the edges from BB except the edge KEEP.  */

static void
remove_all_edges (basic_block bb, edge keep)
{
  remove_all_edges_1 (bb->succs, keep);
  remove_all_edges_1 (bb->preds, keep);
}

/* Rename the SSA_NAMEs used in STMT and that appear in IVSTACK.  */

static void 
graphite_rename_ivs_stmt (gimple stmt, graphite_bb_p gbb, scop_p scop,
			  loop_p old, loop_iv_stack ivstack)
{
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      tree new_iv = NULL;
      name_tree old_iv = get_old_iv_from_ssa_name (scop, old, use);
      
      if (old_iv)
	new_iv = loop_iv_stack_get_iv (ivstack,
				       gbb_loop_index (gbb, old_iv->loop));

      if (new_iv)
	SET_USE (use_p, new_iv);
    }
}

/* Returns true if SSA_NAME is a parameter of SCOP.  */

static bool
is_parameter (scop_p scop, tree ssa_name)
{
  int i;
  VEC (name_tree, heap) *params = SCOP_PARAMS (scop);
  name_tree param;

  for (i = 0; VEC_iterate (name_tree, params, i, param); i++)
    if (param->t == ssa_name)
      return true;

  return false;
}

/* Returns true if NAME is an old induction variable in SCOP.  OLD is
   the original loop that contained the definition of NAME.  */

static bool
is_old_iv (scop_p scop, loop_p old, tree name)
{
  return get_old_iv_from_ssa_name (scop, old, name) != NULL;

}

static void expand_scalar_variables_stmt (gimple, graphite_bb_p, scop_p, loop_p,
					  loop_iv_stack);

/* Constructs a tree which only contains old_ivs and parameters.  Any
   other variables that are defined outside GBB will be eliminated by
   using their definitions in the constructed tree.  OLD_LOOP_FATHER
   is the original loop that contained GBB.  */

static tree
expand_scalar_variables_expr (tree type, tree op0, enum tree_code code, 
			      tree op1, graphite_bb_p gbb, scop_p scop, 
			      loop_p old_loop_father, loop_iv_stack ivstack)
{
  if (TREE_CODE_CLASS (code) == tcc_constant
      && code == INTEGER_CST)
      return op0;

  if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = 
	expand_scalar_variables_expr (op0_type, op0, op0_code,
				      NULL, gbb, scop, old_loop_father,
				      ivstack);

      return fold_build1 (code, type, op0_expr);
    }

  if (TREE_CODE_CLASS (code) == tcc_binary)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = 
	expand_scalar_variables_expr (op0_type, op0, op0_code,
				      NULL, gbb, scop, old_loop_father,
				      ivstack);
      tree op1_type = TREE_TYPE (op1);
      enum tree_code op1_code = TREE_CODE (op1);
      tree op1_expr = 
	expand_scalar_variables_expr (op1_type, op1, op1_code,
				      NULL, gbb, scop, old_loop_father,
				      ivstack);

      return fold_build2 (code, type, op0_expr, op1_expr);
    }

  if (code == SSA_NAME)
    {
      tree var0, var1;
      gimple def_stmt;
      enum tree_code subcode;
      
      if(is_parameter (scop, op0) ||
	 is_old_iv (scop, old_loop_father, op0))
	return op0;
      
      def_stmt = SSA_NAME_DEF_STMT (op0);
      
      if (gimple_bb (def_stmt) == GBB_BB (gbb))
	{
	  /* If the defining statement is in the basic block already
	     we do not need to create a new expression for it, we
	     only need to ensure its operands are expanded.  */
	  expand_scalar_variables_stmt (def_stmt, gbb, scop,
					old_loop_father, ivstack);
	  return op0;
	  
	}
      else
	{
	  if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	    return op0;
	  
	  var0 = gimple_assign_rhs1 (def_stmt);
	  subcode = gimple_assign_rhs_code (def_stmt);
	  var1 = gimple_assign_rhs2 (def_stmt);
	  
	  return expand_scalar_variables_expr (type, var0, subcode, var1, 
					       gbb, scop, old_loop_father, 
					       ivstack);
	}
    }

  gcc_unreachable ();
  return NULL;
}

/* Replicates any uses of non-parameters and non-old-ivs variablesthat
   are defind outside GBB with code that is inserted in GBB.
   OLD_LOOP_FATHER is the original loop that contained STMT.  */
 
static void
expand_scalar_variables_stmt (gimple stmt, graphite_bb_p gbb, scop_p scop,
			      loop_p old_loop_father, loop_iv_stack ivstack)
{
  ssa_op_iter iter;
  use_operand_p use_p;
  basic_block bb = GBB_BB (gbb);

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      tree type = TREE_TYPE (use);
      enum tree_code code  = TREE_CODE (use);
      tree use_expr = expand_scalar_variables_expr (type, use, code, NULL,
						    gbb, scop, old_loop_father, 
						    ivstack);
      if (use_expr != use)
	{
	  gimple_stmt_iterator gsi = gsi_after_labels (bb);
	  tree new_use =
	    force_gimple_operand_gsi (&gsi, use_expr, true, NULL,
				      true, GSI_NEW_STMT);
	  SET_USE (use_p, new_use);
	}
    }
}

/* Copies the definitions outside of GBB of variables that are not
   induction variables nor parameters. GBB must only contain
   "external" references to these types of variables.  OLD_LOOP_FATHER
   is the original loop that contained GBB.  */

static void 
expand_scalar_variables (graphite_bb_p gbb, scop_p scop, 
			 loop_p old_loop_father, loop_iv_stack ivstack)
{
  basic_block bb = GBB_BB (gbb);
  gimple_stmt_iterator gsi;
  
  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);
      expand_scalar_variables_stmt (stmt, gbb, scop, old_loop_father, 
				    ivstack); 
      gsi_next (&gsi);
    }
}

/* Rename all the SSA_NAMEs from block GBB that appear in IVSTACK in
   terms of new induction variables.  OLD_LOOP_FATHER is the original
   loop that contained GBB.  */

static void 
graphite_rename_ivs (graphite_bb_p gbb, scop_p scop, loop_p old_loop_father,
		     loop_iv_stack ivstack)
{
  basic_block bb = GBB_BB (gbb);
  gimple_stmt_iterator gsi;
  
  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);

      if (gimple_get_lhs (stmt)
	  && TREE_CODE (gimple_get_lhs (stmt)) == SSA_NAME
	  && get_old_iv_from_ssa_name (scop, old_loop_father,
				       gimple_get_lhs (stmt)))
	gsi_remove (&gsi, false);
      else
	{
	  graphite_rename_ivs_stmt (stmt, gbb, scop, old_loop_father, ivstack); 
	  gsi_next (&gsi);
	}
    }
}

/* Move all the PHI nodes from block FROM to block TO.
   OLD_LOOP_FATHER is the original loop that contained FROM.  */

static void
move_phi_nodes (scop_p scop, loop_p old_loop_father, basic_block from,
		basic_block to)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (from); !gsi_end_p (gsi);)
    {
      gimple phi = gsi_stmt (gsi);
      tree op = gimple_phi_result (phi);

      if (get_old_iv_from_ssa_name (scop, old_loop_father, op) == NULL)
	{
	  gimple new_phi = make_phi_node (op, 0);
	  add_phi_node_to_bb (new_phi, to);
	}
      remove_phi_node (&gsi, false);
    }
}

/* Remove condition from BB.  */

static void
remove_condition (basic_block bb)
{
  gimple last = last_stmt (bb);

  if (last && gimple_code (last) == GIMPLE_COND)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      gsi_remove (&gsi, true);
    }
}

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag set.  */

static edge
get_true_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_TRUE_VALUE) 
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Translates a CLAST statement STMT to GCC representation.  NEXT_E is
   the edge where new generated code should be attached.  BB_EXIT is the last
   basic block that defines the scope of code generation.  CONTEXT_LOOP is the
   loop in which the generated code will be placed (might be NULL).  */

static edge
translate_clast (scop_p scop, struct loop *context_loop,
		 struct clast_stmt *stmt, edge next_e, loop_iv_stack ivstack)
{
  if (!stmt)
    return next_e;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    return translate_clast (scop, context_loop, stmt->next, next_e, ivstack);

  if (CLAST_STMT_IS_A (stmt, stmt_user))
    {
      CloogStatement *cs = ((struct clast_user_stmt *) stmt)->statement;
      graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);
      basic_block bb = gbb->bb;
      loop_p old_loop_father = bb->loop_father;

      if (bb == ENTRY_BLOCK_PTR)
	return next_e;

      remove_condition (bb);
      expand_scalar_variables (gbb, scop, old_loop_father, ivstack);
      remove_all_edges (bb, next_e);
      move_phi_nodes (scop, old_loop_father, bb, next_e->src);	
      redirect_edge_succ_nodup (next_e, bb);

      if (context_loop)
	{
	  remove_bb_from_loops (bb);
	  add_bb_to_loop (bb, context_loop);
	}

      set_immediate_dominator (CDI_DOMINATORS, next_e->dest, next_e->src); 
      mark_virtual_ops_in_bb (bb);
      next_e = make_edge (bb,
			  context_loop ? context_loop->latch : EXIT_BLOCK_PTR,
			  EDGE_FALLTHRU);;
      graphite_rename_ivs (gbb, scop, old_loop_father, ivstack);
      return translate_clast (scop, context_loop, stmt->next, next_e, ivstack);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    {
      struct loop *loop
	= graphite_create_new_loop (scop, next_e, (struct clast_for *) stmt,
				    ivstack, context_loop ? context_loop
				    : get_loop (0));
      edge last_e = single_exit (loop);
	
      next_e = translate_clast (scop, loop, ((struct clast_for *) stmt)->body,
				single_pred_edge (loop->latch), ivstack);
      redirect_edge_succ_nodup (next_e, loop->latch);
	
      set_immediate_dominator (CDI_DOMINATORS, next_e->dest, next_e->src);
      loop_iv_stack_pop (ivstack);

      return translate_clast (scop, context_loop, stmt->next, last_e, ivstack);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    {
      edge last_e = graphite_create_new_guard (scop, next_e,
					       ((struct clast_guard *) stmt),
					       ivstack);
      edge true_e = get_true_edge_from_guard_bb (next_e->dest);
      next_e = translate_clast (scop, context_loop, 
				((struct clast_guard *) stmt)->then,
				true_e, ivstack);
      redirect_edge_succ_nodup (next_e, last_e->src);
      return translate_clast (scop, context_loop, stmt->next, last_e, ivstack);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    {
      next_e = translate_clast (scop, context_loop,
				((struct clast_block *) stmt)->body,
				next_e, ivstack);
      return translate_clast (scop, context_loop, stmt->next, next_e, ivstack);
    }

  gcc_unreachable ();
}

/* Build cloog program for SCoP.  */

static void
build_cloog_prog (scop_p scop)
{
  int i;
  int max_nb_loops = scop_max_loop_depth (scop);
  graphite_bb_p gbb;
  CloogLoop *loop_list = NULL;
  CloogBlockList *block_list = NULL;
  CloogDomainList *scattering = NULL;
  CloogProgram *prog = SCOP_PROG (scop);
  int nbs = 2 * max_nb_loops + 1;
  int *scaldims = (int *) xmalloc (nbs * (sizeof (int)));

  cloog_program_set_nb_scattdims (prog, nbs);
  initialize_cloog_names (scop);

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gbb); i++)
    {
      /* Build new block.  */
      CloogMatrix *domain = GBB_DOMAIN (gbb);
      CloogStatement *stmt = cloog_statement_alloc (GBB_BB (gbb)->index);
      CloogBlock *block = cloog_block_alloc (stmt, 0, NULL,
					     nb_loops_around_gb (gbb));
      cloog_statement_set_usr (stmt, gbb);

      /* Add empty domain to all bbs, which do not yet have a domain, as they
         are not part of any loop.  */
      if (domain == NULL)
      	{
          domain = cloog_matrix_alloc (0, scop_nb_params (scop) + 2);
          GBB_DOMAIN (gbb) = domain;
	}

      /* Build loop list.  */
      {
        CloogLoop *new_loop_list = cloog_loop_malloc ();
        cloog_loop_set_next (new_loop_list, loop_list);
        cloog_loop_set_domain (new_loop_list,
			       cloog_domain_matrix2domain (domain));
        cloog_loop_set_block (new_loop_list, block);
        loop_list = new_loop_list;
      }

      /* Build block list.  */
      {
        CloogBlockList *new_block_list = cloog_block_list_malloc ();

        cloog_block_list_set_next (new_block_list, block_list);
        cloog_block_list_set_block (new_block_list, block);
        block_list = new_block_list;
      }

      /* Build scattering list.  */
      {
        /* XXX: Replace with cloog_domain_list_alloc(), when available.  */
        CloogDomainList *new_scattering
	  = (CloogDomainList *) xmalloc (sizeof (CloogDomainList));
        CloogMatrix *scat_mat = schedule_to_scattering (gbb, nbs);

        cloog_set_next_domain (new_scattering, scattering);
        cloog_set_domain (new_scattering,
			  cloog_domain_matrix2domain (scat_mat));
        scattering = new_scattering;
        cloog_matrix_free (scat_mat);
      }
    }

  cloog_program_set_loop (prog, loop_list);
  cloog_program_set_blocklist (prog, block_list);

  for (i = 0; i < nbs; i++)
    scaldims[i] = 0 ;

  cloog_program_set_scaldims (prog, scaldims);

  /* Extract scalar dimensions to simplify the code generation problem.  */
  cloog_program_extract_scalars (prog, scattering);

  /* Apply scattering.  */
  cloog_program_scatter (prog, scattering);

  /* Iterators corresponding to scalar dimensions have to be extracted.  */
  cloog_names_scalarize (cloog_program_names (prog), nbs,
			 cloog_program_scaldims (prog));
  
  /* Free blocklist.  */
  {
    CloogBlockList *next = cloog_program_blocklist (prog);
	
    while (next)
      {
        CloogBlockList *toDelete = next;
        next = cloog_block_list_next (next);
        cloog_block_list_set_next (toDelete, NULL);
        cloog_block_list_set_block (toDelete, NULL);
        cloog_block_list_free (toDelete);
      }
    cloog_program_set_blocklist (prog, NULL);
  }
}

/* Return the options that will be used in GLOOG.  */

static CloogOptions *
set_cloog_options (void)
{
  CloogOptions *options = cloog_options_malloc ();

  /* Change cloog output language to C.  If we do use FORTRAN instead, cloog
     will stop e.g. with "ERROR: unbounded loops not allowed in FORTRAN.", if
     we pass an incomplete program to cloog.  */
  options->language = LANGUAGE_C;

  /* Enable complex equality spreading: removes dummy statements
     (assignments) in the generated code which repeats the
     substitution equations for statements.  This is useless for
     GLooG.  */
  options->esp = 1;

  /* Enable C pretty-printing mode: normalizes the substitution
     equations for statements.  */
  options->cpp = 1;

  /* Allow cloog to build strides with a stride width different to one.
     This example has stride = 4:

     for (i = 0; i < 20; i += 4)
       A  */
  options->strides = 1;

  /* Disable optimizations and make cloog generate source code closer to the
     input.  This is useful for debugging,  but later we want the optimized
     code.

     XXX: We can not disable optimizations, as loop blocking is not working
     without them.  */
  if (0)
    {
      options->f = -1;
      options->l = INT_MAX;
    }

  return options;
}

/* Prints STMT to STDERR.  */

void
debug_clast_stmt (struct clast_stmt *stmt)
{
  CloogOptions *options = set_cloog_options ();

  pprint (stderr, stmt, 0, options);
}

/* Find the right transform for the SCOP, and return a Cloog AST
   representing the new form of the program.  */

static struct clast_stmt *
find_transform (scop_p scop)
{
  CloogProgram *prog;
  struct clast_stmt *stmt;
  CloogOptions *options = set_cloog_options ();

  /* Connect new cloog prog generation to graphite.  */
  build_cloog_prog (scop);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Cloog Input [\n");
      cloog_program_print (dump_file, SCOP_PROG(scop));
      fprintf (dump_file, "]\n");
    }

  prog = cloog_program_generate (SCOP_PROG (scop), options);
  stmt = cloog_clast_create (prog, options);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Cloog Output[\n");
      pprint (dump_file, stmt, 0, options);
      cloog_program_dump_cloog (dump_file, prog);
      fprintf (dump_file, "]\n");
    }

  cloog_options_free (options);
  return stmt;
}

/* Return a vector of all the virtual phi nodes in the current
   function.  */
 
static VEC (gimple, heap) *
collect_virtual_phis (void)     
{
  gimple_stmt_iterator si;
  gimple_vec phis = VEC_alloc (gimple, heap, 3);
  basic_block bb;

  FOR_EACH_BB (bb) 
    for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
      /* The phis we moved will have 0 arguments because the
	 original edges were removed.  */
      if (gimple_phi_num_args (gsi_stmt (si)) == 0)
	VEC_safe_push (gimple, heap, phis, gsi_stmt (si));

  /* Deallocate if we did not find any.  */
  if (VEC_length (gimple, phis) == 0)
    {
      VEC_free (gimple, heap, phis);
      phis = NULL;
    }

  return phis;
}

/* Find a virtual definition for variable VAR in BB.  */

static tree
find_vdef_for_var_in_bb (basic_block bb, tree var)
{
  gimple_stmt_iterator gsi;
  gimple phi;
  def_operand_p def_var;
  vuse_vec_p vv;
  ssa_op_iter op_iter;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    FOR_EACH_SSA_VDEF_OPERAND (def_var, vv, gsi_stmt (gsi), op_iter)
      if (SSA_NAME_VAR (*def_var) == var)
	return *def_var;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    FOR_EACH_SSA_DEF_OPERAND (def_var, gsi_stmt (gsi), op_iter, SSA_OP_DEF)
      if (SSA_NAME_VAR (*def_var) == var)
	return *def_var;

  for (gsi = gsi_start_phis (bb); !gsi_end_p(gsi); gsi_next (&gsi))
    {
      phi = gsi_stmt (gsi);
      if (SSA_NAME_VAR (PHI_RESULT (phi)) == var)
	return PHI_RESULT (phi);
    }

  return NULL;
}

/* Recursive helper.  */

static tree
find_vdef_for_var_1 (basic_block bb, struct pointer_set_t *visited, tree var)
{
  tree result = NULL;
  edge_iterator ei;
  edge pred_edge;

  if (pointer_set_contains (visited, bb))
    return NULL;

  pointer_set_insert (visited, bb);
  result = find_vdef_for_var_in_bb (bb, var);

  if (!result)
    FOR_EACH_EDGE (pred_edge, ei, bb->preds)
      if (!result)
	result = find_vdef_for_var_1 (pred_edge->src, visited, var);

  return result;
}

/* Finds a virtual definition for variable VAR.  */

static tree
find_vdef_for_var (basic_block bb, tree var)
{
  struct pointer_set_t *visited = pointer_set_create ();
  tree def = find_vdef_for_var_1 (bb, visited, var);

  pointer_set_destroy (visited);
  return def;
}

/* Update the virtual phis after loop bodies are moved to new
   loops.  */

static void
patch_phis_for_virtual_defs (void)
{
  int i;
  gimple phi;
  VEC (gimple, heap) *virtual_phis = collect_virtual_phis ();
  
  for (i = 0; VEC_iterate (gimple, virtual_phis, i, phi); i++)
    {
      basic_block bb = gimple_bb (phi);
      edge_iterator ei;
      edge pred_edge;
      gimple_stmt_iterator gsi;
      gimple new_phi;
      tree phi_result = PHI_RESULT (phi);
      tree var = SSA_NAME_VAR (phi_result);

      new_phi = create_phi_node (phi_result, bb);
      SSA_NAME_DEF_STMT (phi_result) = new_phi;

      FOR_EACH_EDGE (pred_edge, ei, bb->preds)
	{
	  tree def = find_vdef_for_var (pred_edge->src, var);

	  if (def)
	    add_phi_arg (new_phi, def, pred_edge);
	  else
	    add_phi_arg (new_phi, gimple_default_def (cfun, var), pred_edge);
	}

      gsi = gsi_for_stmt (phi);
      remove_phi_node (&gsi, false);
    }
}

/* Mark the original loops of SCOP for removal, replacing their header
   field with NULL.  */

static void
mark_old_loops (scop_p scop)
{
  int i;
  struct loop *loop;

  for (i = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), i, loop); i++)
    {
      loop->header = NULL;
      loop->latch = NULL;
    }
}

/* Scan the loops and remove the ones that have been marked for
   removal.  */

static void
remove_dead_loops (void)
{
  struct loop *loop, *ploop;
  loop_iterator li;

  FOR_EACH_LOOP (li, loop, LI_FROM_INNERMOST)
    {
      /* Remove only those loops that we marked to be removed with
	 mark_old_loops.  */
      if (loop->header)
	continue;

      while (loop->inner)
	{
	  ploop = loop->inner;
	  flow_loop_tree_node_remove (ploop);
	  flow_loop_tree_node_add (loop_outer (loop), ploop);
	}

      /* Remove the loop and free its data.  */
      delete_loop (loop);
    }
}

/* Returns true when it is possible to generate code for this STMT.
   For the moment we cannot generate code when Cloog decides to
   duplicate a statement, as we do not do a copy, but a move.
   USED_BASIC_BLOCKS records the blocks that have already been seen.
   We return false if we have to generate code twice for the same
   block.  */

static bool 
can_generate_code_stmt (struct clast_stmt *stmt,
			struct pointer_set_t *used_basic_blocks)
{
  if (!stmt)
    return true;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    return can_generate_code_stmt (stmt->next, used_basic_blocks);

  if (CLAST_STMT_IS_A (stmt, stmt_user))
    {
      CloogStatement *cs = ((struct clast_user_stmt *) stmt)->statement;
      graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);

      if (pointer_set_contains (used_basic_blocks, gbb))
	return false;
      pointer_set_insert (used_basic_blocks, gbb);
      return can_generate_code_stmt (stmt->next, used_basic_blocks);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    return can_generate_code_stmt (((struct clast_for *) stmt)->body,
				   used_basic_blocks)
      && can_generate_code_stmt (stmt->next, used_basic_blocks);

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    return can_generate_code_stmt (((struct clast_guard *) stmt)->then,
				   used_basic_blocks);

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    return can_generate_code_stmt (((struct clast_block *) stmt)->body,
				   used_basic_blocks)
      && can_generate_code_stmt (stmt->next, used_basic_blocks);

  return false;
}

/* Returns true when it is possible to generate code for this STMT.  */

static bool 
can_generate_code (struct clast_stmt *stmt)
{
  bool result;
  struct pointer_set_t *used_basic_blocks = pointer_set_create ();

  result = can_generate_code_stmt (stmt, used_basic_blocks);
  pointer_set_destroy (used_basic_blocks);
  return result;
}

/* Skip any definition that is a phi node with a single phi def.  */

static tree 
skip_phi_defs (tree ssa_name)
{
  tree result = ssa_name;
  gimple def_stmt = SSA_NAME_DEF_STMT (ssa_name);

  if (gimple_code (def_stmt) == GIMPLE_PHI 
      && gimple_phi_num_args (def_stmt) == 1)
    result = skip_phi_defs (gimple_phi_arg(def_stmt,0)->def);

  return result;
}

/* Returns a VEC containing the phi-arg defs coming from SCOP_EXIT in
   the destination block of SCOP_EXIT.  */

static VEC (tree, heap) *
collect_scop_exit_phi_args (edge scop_exit)
{
  VEC (tree, heap) *phi_args = VEC_alloc (tree, heap, 1);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (scop_exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree phi_arg = skip_phi_defs(PHI_ARG_DEF_FROM_EDGE (phi, scop_exit));

      VEC_safe_push (tree, heap, phi_args, phi_arg);
    }

  return phi_args;
}

/* Patches (adds) PHI_ARGS to the phi nodes in SCOP_EXIT destination.  */

static void
patch_scop_exit_phi_args (edge scop_exit,
			  VEC (tree, heap) *phi_args)
{
  int i = 0;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (scop_exit->dest); !gsi_end_p (gsi);
       gsi_next (&gsi), i++)
    {
      tree def = VEC_index (tree, phi_args, i);
      gimple phi = gsi_stmt (gsi);

      gcc_assert (PHI_ARG_DEF_FROM_EDGE (phi, scop_exit) == NULL);

      add_phi_arg (phi, def, scop_exit);
    }
}

/* GIMPLE Loop Generator: generates loops from STMT in GIMPLE form for
   the given SCOP.  */

static void
gloog (scop_p scop, struct clast_stmt *stmt)
{
  edge new_scop_exit_edge = NULL;
  basic_block scop_exit = SCOP_EXIT (scop);
  VEC (tree, heap)* phi_args = 
    collect_scop_exit_phi_args (SESE_EXIT (SCOP_REGION (scop)));
  VEC (name_tree, heap) *ivstack = VEC_alloc (name_tree, heap, 10);
  edge construction_edge = SESE_ENTRY (SCOP_REGION (scop));
  basic_block old_scop_exit_idom = get_immediate_dominator (CDI_DOMINATORS,
							    scop_exit);

  if (!can_generate_code (stmt))
    {
      cloog_clast_free (stmt);
      return;
    }

  new_scop_exit_edge = translate_clast (scop, 
					construction_edge->src->loop_father,
					stmt, construction_edge, &ivstack);
  redirect_edge_succ (new_scop_exit_edge, scop_exit);
  if (!old_scop_exit_idom
      || !dominated_by_p (CDI_DOMINATORS, SCOP_ENTRY (scop),
			  old_scop_exit_idom)
      || SCOP_ENTRY (scop) == old_scop_exit_idom)
    set_immediate_dominator (CDI_DOMINATORS,
			     new_scop_exit_edge->dest,
			     new_scop_exit_edge->src);

  cloog_clast_free (stmt);

  if (new_scop_exit_edge->dest == EXIT_BLOCK_PTR)
    new_scop_exit_edge->flags = 0;
 
  find_unreachable_blocks ();
  delete_unreachable_blocks ();
  patch_phis_for_virtual_defs ();
  patch_scop_exit_phi_args (new_scop_exit_edge, phi_args);
  mark_old_loops (scop);
  remove_dead_loops ();
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa); 

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_dominators (CDI_DOMINATORS);
  verify_ssa (false);
#endif
}

/* Returns the number of data references in SCOP.  */

static int
nb_data_refs_in_scop (scop_p scop)
{
  int i;
  graphite_bb_p gbb;
  int res = 0;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gbb); i++)
    res += VEC_length (data_reference_p, GBB_DATA_REFS (gbb));

  return res;
}

/* Check if a graphite bb can be ignored in graphite.  We ignore all
   bbs, that only contain code, that will be eliminated later.

   TODO: - Move PHI nodes and scalar variables out of these bbs, that only
           remain conditions and induction variables.  */

static bool
gbb_can_be_ignored (graphite_bb_p gb)
{
  gimple_stmt_iterator gsi;
  scop_p scop = GBB_SCOP (gb);
  loop_p loop = GBB_BB (gb)->loop_father;

  if (VEC_length (data_reference_p, GBB_DATA_REFS(gb)))
    return false;

  /* Check statements.  */
  for (gsi = gsi_start_bb (GBB_BB (gb)); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      switch (gimple_code (stmt))
        {
          /* Control flow expressions can be ignored, as they are
             represented in the iteration domains and will be
             regenerated by graphite.  */
          case GIMPLE_COND:
	  case GIMPLE_GOTO:
	  case GIMPLE_SWITCH:
            break;

          /* Scalar variables can be ignored, if we can regenerate
             them later using their scalar evolution function.
             XXX: Just a heuristic, that needs further investigation.  */
          case GIMPLE_ASSIGN:
	    {
	      tree var = gimple_assign_lhs (stmt);
	      var = analyze_scalar_evolution (loop, var);
	      var = instantiate_scev (block_before_scop (scop), loop, var);

	      if (TREE_CODE (var) == SCEV_NOT_KNOWN)
		return false;

	      break;
	    }
          /* Otherwise not ignoreable.  */
          default:
            return false;
        }
    }

  return true;
}

/* Remove all ignoreable gbbs from SCOP.  */

static void
scop_remove_ignoreable_gbbs (scop_p scop)
{
  graphite_bb_p gb;
  int i;

  int max_schedule = scop_max_loop_depth (scop) + 1;
  lambda_vector last_schedule = lambda_vector_new (max_schedule);
  lambda_vector_clear (last_schedule, max_schedule);

  /* Update schedules.  */
  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      int nb_loops = gbb_nb_loops (gb);

      if (GBB_STATIC_SCHEDULE (gb) [nb_loops] == 0)
        last_schedule [nb_loops] = 0;

      if (gbb_can_be_ignored (gb))
        {
          /* Mark gbb for remove.  */
          bitmap_clear_bit (SCOP_BBS_B (scop), gb->bb->index);
          GBB_SCOP (gb) = NULL;
          last_schedule [nb_loops]--;
        }
      else
        lambda_vector_add (GBB_STATIC_SCHEDULE (gb), last_schedule,
                           GBB_STATIC_SCHEDULE (gb), nb_loops + 1);
    }

  /* Remove gbbs.  */
  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    if (GBB_SCOP (gb) == NULL)
      {
        VEC_unordered_remove (graphite_bb_p, SCOP_BBS (scop), i);
        free_graphite_bb (gb);
        /* XXX: Hackish? But working.  */
        i--;
      }  

  graphite_sort_gbbs (scop);
}

/* Move the loop at index LOOP and insert it before index NEW_LOOP_POS.
   This transformartion is only valid, if the loop nest between i and k is
   perfectly nested. Therefore we do not need to change the static schedule.

   Example:

   for (i = 0; i < 50; i++)
     for (j ...)
       for (k = 5; k < 100; k++)
         A

   To move k before i use:

   graphite_trans_bb_move_loop (A, 2, 0)

   for (k = 5; k < 100; k++)
     for (i = 0; i < 50; i++)
       for (j ...)
         A

   And to move k back:

   graphite_trans_bb_move_loop (A, 0, 2)

   This function does not check the validity of interchanging loops.
   This should be checked before calling this function.  */

static void
graphite_trans_bb_move_loop (graphite_bb_p gb, int loop,
			     int new_loop_pos)
{
  CloogMatrix *domain = GBB_DOMAIN (gb);
  int row, j;
  loop_p tmp_loop_p;

  gcc_assert (loop < gbb_nb_loops (gb)
	      && new_loop_pos < gbb_nb_loops (gb));

  /* Update LOOPS vector.  */
  tmp_loop_p = VEC_index (loop_p, GBB_LOOPS (gb), loop);
  VEC_ordered_remove (loop_p, GBB_LOOPS (gb), loop);
  VEC_safe_insert (loop_p, heap, GBB_LOOPS (gb), new_loop_pos, tmp_loop_p);

  /* Move the domain columns.  */
  if (loop < new_loop_pos)
    for (row = 0; row < domain->NbRows; row++)
      {
        Value tmp;
        value_init (tmp);
        value_assign (tmp, domain->p[row][loop + 1]);
   
        for (j = loop ; j < new_loop_pos - 1; j++)
          value_assign (domain->p[row][j + 1], domain->p[row][j + 2]);

        value_assign (domain->p[row][new_loop_pos], tmp);
        value_clear (tmp);
      }
  else
    for (row = 0; row < domain->NbRows; row++)
      {
        Value tmp;
        value_init (tmp);
        value_assign (tmp, domain->p[row][loop + 1]);

        for (j = loop ; j > new_loop_pos; j--)
          value_assign (domain->p[row][j + 1], domain->p[row][j]);
     
        value_assign (domain->p[row][new_loop_pos + 1], tmp);
        value_clear (tmp);
      }
}

/* Get the index of the column representing constants in the DOMAIN
   matrix.  */

static int
const_column_index (CloogMatrix *domain)
{
  return domain->NbColumns - 1;
}


/* Get the first index that is positive or negative, determined
   following the value of POSITIVE, in matrix DOMAIN in COLUMN.  */

static int
get_first_matching_sign_row_index (CloogMatrix *domain, int column,
				   bool positive)
{
  int row;

  for (row = 0; row < domain->NbRows; row++)
    {
      int val = value_get_si (domain->p[row][column]);

      if (val > 0 && positive)
	return row;

      else if (val < 0 && !positive)
	return row;
    }

  gcc_unreachable ();
}

/* Get the lower bound of COLUMN in matrix DOMAIN.  */

static int
get_lower_bound_row (CloogMatrix *domain, int column)
{
  return get_first_matching_sign_row_index (domain, column, true);
}

/* Get the upper bound of COLUMN in matrix DOMAIN.  */

static int
get_upper_bound_row (CloogMatrix *domain, int column)
{
  return get_first_matching_sign_row_index (domain, column, false);
}

/* Get the lower bound of LOOP.  */

static void
get_lower_bound (CloogMatrix *domain, int loop, Value lower_bound_result)
{
  int lower_bound_row = get_lower_bound_row (domain, loop);
  value_init (lower_bound_result);
  value_assign (lower_bound_result,
		domain->p[lower_bound_row][const_column_index(domain)]);
}

/* Get the upper bound of LOOP.  */

static void
get_upper_bound (CloogMatrix *domain, int loop, Value upper_bound_result)
{
  int upper_bound_row = get_upper_bound_row (domain, loop);
  value_init (upper_bound_result);
  value_assign (upper_bound_result,
		domain->p[upper_bound_row][const_column_index(domain)]);
}

/* Strip mines the loop of BB at the position LOOP_DEPTH with STRIDE.
   Always valid, but not always a performance improvement.  */
  
static void
graphite_trans_bb_strip_mine (graphite_bb_p gb, int loop_depth, int stride)
{
  int row, col;

  CloogMatrix *domain = GBB_DOMAIN (gb);
  CloogMatrix *new_domain = cloog_matrix_alloc (domain->NbRows + 3,
                                                domain->NbColumns + 1);   

  int col_loop_old = loop_depth + 2; 
  int col_loop_strip = col_loop_old - 1;

  Value old_lower_bound;
  Value old_upper_bound;


  gcc_assert (loop_depth <= gbb_nb_loops (gb) - 1);

  VEC_safe_insert (loop_p, heap, GBB_LOOPS (gb), loop_depth, NULL);

  GBB_DOMAIN (gb) = new_domain;

  /*
   nrows = 4, ncols = 4
  eq    i    j    c
   1    1    0    0 
   1   -1    0   99 
   1    0    1    0 
   1    0   -1   99 
  */
 
  /* Move domain.  */
  for (row = 0; row < domain->NbRows; row++)
    for (col = 0; col < domain->NbColumns; col++)
      if (col <= loop_depth)
        {
          value_assign (new_domain->p[row][col], domain->p[row][col]);
        }
      else
        {
          value_assign (new_domain->p[row][col + 1], domain->p[row][col]);
        }


  /*
    nrows = 6, ncols = 5
           outer inner
   eq   i   jj    j    c
   1    1    0    0    0 
   1   -1    0    0   99 
   1    0    0    1    0 
   1    0    0   -1   99 
   0    0    0    0    0 
   0    0    0    0    0 
   0    0    0    0    0 
   */

  row = domain->NbRows;

  /* Add outer loop.  */

  get_lower_bound (new_domain, col_loop_old, old_lower_bound);
  get_upper_bound (new_domain, col_loop_old, old_upper_bound);

  /* Set Lower Bound */
  value_set_si (new_domain->p[row][0], 1);
  value_set_si (new_domain->p[row][col_loop_strip], 1);
  value_assign (new_domain->p[row][const_column_index (new_domain)],
		old_lower_bound);
  row++;


  /*
    6 5
   eq   i   jj    j    c
   1    1    0    0    0 
   1   -1    0    0   99 
   1    0    0    1    0  - 
   1    0    0   -1   99   | copy old lower bound
   1    0    1    0    0 <-
   0    0    0    0    0
   0    0    0    0    0
   */

  {
    Value new_upper_bound;
    Value strip_size_value;

    value_init (new_upper_bound);

    value_init (strip_size_value);
    value_set_si (strip_size_value, (int) stride);

  
    value_pdivision(new_upper_bound,old_upper_bound,strip_size_value);
    value_add_int (new_upper_bound, new_upper_bound, 1);

    /* Set Upper Bound */
    value_set_si (new_domain->p[row][0], 1);
    value_set_si (new_domain->p[row][col_loop_strip], -1);
    value_assign (new_domain->p[row][const_column_index (new_domain)],
		  new_upper_bound);
    row++;
  }
  /*
    6 5
   eq   i   jj    j    c
   1    1    0    0    0 
   1   -1    0    0   99 
   1    0    0    1    0  
   1    0    0   -1   99  
   1    0    1    0    0 
   1    0   -1    0   25  (divide old upper bound with stride) 
   0    0    0    0    0
  */

  {
    row = get_lower_bound_row (new_domain, col_loop_old);
    /* Add local variable to keep linear representation.  */
    value_set_si (new_domain->p[row][0], 1);
    value_set_si (new_domain->p[row][const_column_index (new_domain)],0);
    value_set_si (new_domain->p[row][col_loop_old], 1);
    value_set_si (new_domain->p[row][col_loop_strip], -1*((int)stride));
  }

  /*
    6 5
   eq   i   jj    j    c
   1    1    0    0    0 
   1   -1    0    0   99 
   1    0    -1   1    0  
   1    0    0   -1   99  
   1    0    1    0    0 
   1    0   -1    0   25  (divide old upper bound with stride) 
   0    0    0    0    0
  */

  {
    row = new_domain->NbRows-1;
    
    value_set_si (new_domain->p[row][0], 1);
    value_set_si (new_domain->p[row][col_loop_old], -1);
    value_set_si (new_domain->p[row][col_loop_strip], stride);
    value_set_si (new_domain->p[row][const_column_index (new_domain)],
		  stride-1);
  }

  /*
    6 5
   eq   i   jj    j    c
   1    1    0    0    0     i >= 0
   1   -1    0    0   99    99 >= i
   1    0    -4   1    0     j >= 4*jj
   1    0    0   -1   99    99 >= j
   1    0    1    0    0    jj >= 0
   1    0   -1    0   25    25 >= jj
   0    0    4    -1   3  jj+3 >= j
  */

  cloog_matrix_free (domain);

  /* Update static schedule.  */
  {
    int i;
    int nb_loops = gbb_nb_loops (gb);
    lambda_vector new_schedule = lambda_vector_new (nb_loops + 1);

    for (i = 0; i <= loop_depth; i++)
      new_schedule[i] = GBB_STATIC_SCHEDULE (gb)[i];  

    for (i = loop_depth + 1; i <= nb_loops - 2; i++)
      new_schedule[i + 2] = GBB_STATIC_SCHEDULE (gb)[i];  

    GBB_STATIC_SCHEDULE (gb) = new_schedule;
  }
}

/* Returns true when the strip mining of LOOP_INDEX by STRIDE is
   profitable or undecidable.  GB is the statement around which the
   loops will be strip mined.  */

static bool
strip_mine_profitable_p (graphite_bb_p gb, int stride,
			 int loop_index)
{
  bool res = true;
  edge exit = NULL;
  tree niter;
  loop_p loop;
  long niter_val;

  loop = VEC_index (loop_p, GBB_LOOPS (gb), loop_index);
  exit = single_exit (loop);

  niter = find_loop_niter (loop, &exit);
  if (niter == chrec_dont_know 
      || TREE_CODE (niter) != INTEGER_CST)
    return true;
  
  niter_val = int_cst_value (niter);

  if (niter_val < stride)
    {
      res = false;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nStrip Mining is not profitable for loop %d:",
		   loop_index);
	  fprintf (dump_file, "number of iterations is too low.\n");
	}
    }
  
  return res;
}
 
/* Determines when the interchange of LOOP_A and LOOP_B belonging to
   SCOP is legal.  */

static bool
is_interchange_valid (scop_p scop, int loop_a, int loop_b)
{
  bool res;
  VEC (ddr_p, heap) *dependence_relations;
  VEC (data_reference_p, heap) *datarefs;

  struct loop *nest = VEC_index (loop_p, SCOP_LOOP_NEST (scop), loop_a);
  int depth = perfect_loop_nest_depth (nest);
  lambda_trans_matrix trans;

  gcc_assert (loop_a < loop_b);

  dependence_relations = VEC_alloc (ddr_p, heap, 10 * 10);
  datarefs = VEC_alloc (data_reference_p, heap, 10);

  if (!compute_data_dependences_for_loop (nest, true, &datarefs,
					  &dependence_relations))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_ddrs (dump_file, dependence_relations);

  trans = lambda_trans_matrix_new (depth, depth);
  lambda_matrix_id (LTM_MATRIX (trans), depth);

  lambda_matrix_row_exchange (LTM_MATRIX (trans), 0, loop_b - loop_a);

  if (!lambda_transform_legal_p (trans, depth, dependence_relations))
    {
      lambda_matrix_row_exchange (LTM_MATRIX (trans), 0, loop_b - loop_a);
      res = false;
    }
  else
    res = true;

  free_dependence_relations (dependence_relations);
  free_data_refs (datarefs);
  return res;
}

/* Loop block the LOOPS innermost loops of GB with stride size STRIDE. 

   Example

   for (i = 0; i <= 50; i++=4) 
     for (k = 0; k <= 100; k++=4) 
       for (l = 0; l <= 200; l++=4) 
         A

   To strip mine the two inner most loops with stride = 4 call:

   graphite_trans_bb_block (A, 4, 2) 

   for (i = 0; i <= 50; i++) 
     for (kk = 0; kk <= 100; kk+=4) 
       for (ll = 0; ll <= 200; ll+=4) 
         for (k = kk; k <= min (100, kk + 3); k++) 
           for (l = ll; l <= min (200, ll + 3); l++) 
             A
*/

static bool
graphite_trans_bb_block (graphite_bb_p gb, int stride, int loops)
{
  int i, j;
  int nb_loops = gbb_nb_loops (gb);
  int start = nb_loops - loops;
  scop_p scop = GBB_SCOP (gb);

  gcc_assert (scop_contains_loop (scop, gbb_loop (gb)));

  for (i = start ; i < nb_loops; i++)
    for (j = i + 1; j < nb_loops; j++)
      if (!is_interchange_valid (scop, i, j))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "\nInterchange not valid for loops %d and %d:\n", i, j);
	  return false;
	}
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\nInterchange valid for loops %d and %d:\n", i, j);

  /* Check if strip mining is profitable for every loop.  */
  for (i = 0; i < nb_loops - start; i++)
    if (!strip_mine_profitable_p (gb, stride, start + i))
      return false;

  /* Strip mine loops.  */
  for (i = 0; i < nb_loops - start; i++)
    graphite_trans_bb_strip_mine (gb, start + 2 * i, stride);

  /* Interchange loops.  */
  for (i = 1; i < nb_loops - start; i++)
    graphite_trans_bb_move_loop (gb, start + 2 * i, start + i);

  return true;
}

/* Loop block LOOPS innermost loops of a loop nest.  BBS represent the
   basic blocks that belong to the loop nest to be blocked.  */

static bool
graphite_trans_loop_block (VEC (graphite_bb_p, heap) *bbs, int loops)
{
  graphite_bb_p gb;
  int i;
  bool transform_done = false;

  /* TODO: - Calculate the stride size automatically.  */
  int stride_size = 64;

  for (i = 0; VEC_iterate (graphite_bb_p, bbs, i, gb); i++)
    transform_done |= graphite_trans_bb_block (gb, stride_size, loops);

  return transform_done;
}

/* Loop block all basic blocks of SCOP.  */

static bool
graphite_trans_scop_block (scop_p scop)
{
  graphite_bb_p gb;
  int i, j;
  int last_nb_loops;
  int nb_loops;
  bool perfect = true;
  bool transform_done = false;

  VEC (graphite_bb_p, heap) *bbs = VEC_alloc (graphite_bb_p, heap, 3);
  int max_schedule = scop_max_loop_depth (scop) + 1;
  lambda_vector last_schedule = lambda_vector_new (max_schedule);

  if (VEC_length (graphite_bb_p, SCOP_BBS (scop)) == 0)
    return transform_done;

  /* Get the data of the first bb.  */
  gb = VEC_index (graphite_bb_p, SCOP_BBS (scop), 0); 
  last_nb_loops = gbb_nb_loops (gb);
  lambda_vector_copy (GBB_STATIC_SCHEDULE (gb), last_schedule,
                      last_nb_loops + 1);
  VEC_safe_push (graphite_bb_p, heap, bbs, gb);
  
  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      /* We did the first bb before.  */
      if (i == 0)
        continue;

      nb_loops = gbb_nb_loops (gb);

      /* If the number of loops is unchanged and only the last element of the
         schedule changes, we stay in the loop nest.  */
      if (nb_loops == last_nb_loops 
          && (last_schedule [nb_loops + 1]
              != GBB_STATIC_SCHEDULE (gb)[nb_loops + 1]))
        {
          VEC_safe_push (graphite_bb_p, heap, bbs, gb);
          continue;
        }

      /* Otherwise, we left the innermost loop. So check, if the last bb was in
         a perfect loop nest and how many loops are contained in this perfect
         loop nest. 
         
         Count the number of zeros from the end of the schedule. They are the
         number of surrounding loops.

         Example:
         last_bb  2 3 2 0 0 0 0 3
         bb       2 4 0
	 <------  j = 4
        
         last_bb  2 3 2 0 0 0 0 3
         bb       2 3 2 0 1
	 <--  j = 2

         If there is no zero, there were other bbs in outer loops and the loop
         nest is not perfect.  */
      for (j = last_nb_loops - 1; j >= 0; j--)
        {
          if (last_schedule [j] != 0
              || (j <= nb_loops && GBB_STATIC_SCHEDULE (gb)[j] == 1))
            {
              j--;
              break;
            }
        }
      
      j++;

      /* Found perfect loop nest.  */
      if (perfect && last_nb_loops - j > 0)
        transform_done |= graphite_trans_loop_block (bbs, last_nb_loops - j);
 
      /* Check if we start with a new loop.

         Example:
  
         last_bb  2 3 2 0 0 0 0 3
         bb       2 3 2 0 0 1 0
        
         Here we start with the loop "2 3 2 0 0 1" 

         last_bb  2 3 2 0 0 0 0 3
         bb       2 3 2 0 0 1 

         But here not, so the loop nest can never be perfect.  */

      perfect = (GBB_STATIC_SCHEDULE (gb)[nb_loops] == 0);

      /* Update the last_bb infos.  We do not do that for the bbs in the same
         loop, as the data we use is not changed.  */
      last_nb_loops = nb_loops;
      lambda_vector_copy (GBB_STATIC_SCHEDULE (gb), last_schedule,
                          nb_loops + 1);
      VEC_truncate (graphite_bb_p, bbs, 0);
      VEC_safe_push (graphite_bb_p, heap, bbs, gb);
    }

  /* Check if the last loop nest was perfect.  It is the same check as above,
     but the comparison with the next bb is missing.  */
  for (j = last_nb_loops - 1; j >= 0; j--)
    if (last_schedule [j] != 0)
      {
	j--;
	break;
      }

  j++;

  /* Found perfect loop nest.  */
  if (last_nb_loops - j > 0)
    transform_done |= graphite_trans_loop_block (bbs, last_nb_loops - j);
  VEC_free (graphite_bb_p, heap, bbs);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nLoop blocked.\n");

  return transform_done;
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

static bool
graphite_apply_transformations (scop_p scop)
{
  bool transform_done = false;

  /* Sort the list of bbs.  Keep them always sorted.  */
  graphite_sort_gbbs (scop);
  scop_remove_ignoreable_gbbs (scop);

  if (flag_loop_block)
    transform_done = graphite_trans_scop_block (scop);

#if 0 && ENABLE_CHECKING
  /* When the compiler is configured with ENABLE_CHECKING, always
     generate code, even if we did not apply any transformation.  This
     provides better code coverage of the backend code generator.

     This also allows to check the performance for an identity
     transform: GIMPLE -> GRAPHITE -> GIMPLE; and the output of CLooG
     is never an identity: if CLooG optimizations are not disabled,
     the CLooG output is always optimized in control flow.  */
  transform_done = true;
#endif

  return transform_done;
}

/* We limit all SCoPs to SCoPs, that are completely surrounded by a loop. 

   Example:

   for (i      |
     {         |
       for (j  |  SCoP 1
       for (k  |
     }         |

   * SCoP frontier, as this line is not surrounded by any loop. *

   for (l      |  SCoP 2

   This is necessary as scalar evolution and parameter detection need a
   outermost loop to initialize parameters correctly.  
  
   TODO: FIX scalar evolution and parameter detection to allow more flexible
         SCoP frontiers.  */

static void
limit_scops (void)
{
  VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap, 3);

  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, current_scops, i, scop); i++)
    {
      int j;
      loop_p loop;
      build_scop_bbs (scop);
      build_scop_loop_nests (scop);

      for (j = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), j, loop); j++) 
        if (!loop_in_scop_p (loop_outer (loop), scop))
          {
	    sd_region open_scop;
	    open_scop.entry = loop_preheader_edge (loop)->dest;
	    open_scop.exit = single_exit (loop)->dest;
	    VEC_safe_push (sd_region, heap, tmp_scops, &open_scop);
	  }
    }

  free_scops (current_scops);
  current_scops = VEC_alloc (scop_p, heap, 3);

  create_sese_edges (tmp_scops);
  build_graphite_scops (tmp_scops);
}

/* Perform a set of linear transforms on the loops of the current
   function.  */

void
graphite_transform_loops (void)
{
  int i;
  scop_p scop;

  if (number_of_loops () <= 1)
    return;

  current_scops = VEC_alloc (scop_p, heap, 3);

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Graphite loop transformations \n");

  cloog_initialize ();
  build_scops ();
  limit_scops ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nnumber of SCoPs: %d\n",
	     VEC_length (scop_p, current_scops));

  for (i = 0; VEC_iterate (scop_p, current_scops, i, scop); i++)
    {
      build_scop_bbs (scop);
      build_scop_loop_nests (scop);
      build_scop_canonical_schedules (scop);
      build_bb_loops (scop);
      find_scop_parameters (scop);
      build_scop_context (scop);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n(In SCoP %d:\n", i);
	  fprintf (dump_file, "\nnumber of bbs: %d\n",
		   VEC_length (graphite_bb_p, SCOP_BBS (scop)));
	  fprintf (dump_file, "\nnumber of loops: %d)\n",
		   VEC_length (loop_p, SCOP_LOOP_NEST (scop)));
	}

      if (!build_scop_iteration_domain (scop))
	continue;

      build_scop_conditions (scop);
      build_scop_data_accesses (scop);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  int nbrefs = nb_data_refs_in_scop (scop);
	  fprintf (dump_file, "\nnumber of data refs: %d\n", nbrefs);
	}

      if (graphite_apply_transformations (scop))
        gloog (scop, find_transform (scop));
    }

  /* Cleanup.  */
  free_scops (current_scops);
  cloog_finalize ();
}

#else /* If Cloog is not available: #ifndef HAVE_cloog.  */

void
graphite_transform_loops (void)
{
  sorry ("Graphite loop optimizations cannot be used");
}

#endif
