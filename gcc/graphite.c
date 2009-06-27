/* Gimple Represented as Polyhedra.
   Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include "value-prof.h"
#include "pointer-set.h"
#include "gimple.h"

#ifdef HAVE_cloog

/* The CLooG header file is not -Wc++-compat ready as of 2009-05-11.
   This #pragma should be removed when it is ready.  */
#if GCC_VERSION >= 4003
#pragma GCC diagnostic warning "-Wc++-compat"
#endif

#include "cloog/cloog.h"
#include "graphite.h"

static VEC (scop_p, heap) *current_scops;

/* Converts a GMP constant V to a tree and returns it.  */

static tree
gmp_cst_to_tree (tree type, Value v)
{
  return build_int_cst (type, value_get_si (v));
}

/* Returns true when BB is in REGION.  */

static bool
bb_in_sese_p (basic_block bb, sese region)
{
  return pointer_set_contains (SESE_REGION_BBS (region), bb);
}

/* Returns true when LOOP is in the SESE region R.  */

static inline bool 
loop_in_sese_p (struct loop *loop, sese r)
{
  return (bb_in_sese_p (loop->header, r)
	  && bb_in_sese_p (loop->latch, r));
}

/* For a USE in BB, if BB is outside REGION, mark the USE in the
   SESE_LIVEIN and SESE_LIVEOUT sets.  */

static void
sese_build_livein_liveouts_use (sese region, basic_block bb, tree use)
{
  unsigned ver;
  basic_block def_bb;

  if (TREE_CODE (use) != SSA_NAME)
    return;

  ver = SSA_NAME_VERSION (use);
  def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));
  if (!def_bb
      || !bb_in_sese_p (def_bb, region)
      || bb_in_sese_p (bb, region))
    return;

  if (!SESE_LIVEIN_VER (region, ver))
    SESE_LIVEIN_VER (region, ver) = BITMAP_ALLOC (NULL);

  bitmap_set_bit (SESE_LIVEIN_VER (region, ver), bb->index);
  bitmap_set_bit (SESE_LIVEOUT (region), ver);
}

/* Marks for rewrite all the SSA_NAMES defined in REGION and that are
   used in BB that is outside of the REGION.  */

static void
sese_build_livein_liveouts_bb (sese region, basic_block bb)
{
  gimple_stmt_iterator bsi;
  edge e;
  edge_iterator ei;
  ssa_op_iter iter;
  tree var;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi); gsi_next (&bsi))
      sese_build_livein_liveouts_use (region, bb,
				      PHI_ARG_DEF_FROM_EDGE (gsi_stmt (bsi), e));

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    FOR_EACH_SSA_TREE_OPERAND (var, gsi_stmt (bsi), iter, SSA_OP_ALL_USES)
      sese_build_livein_liveouts_use (region, bb, var);
}

/* Build the SESE_LIVEIN and SESE_LIVEOUT for REGION.  */

void
sese_build_livein_liveouts (sese region)
{
  basic_block bb;

  SESE_LIVEOUT (region) = BITMAP_ALLOC (NULL);
  SESE_NUM_VER (region) = num_ssa_names;
  SESE_LIVEIN (region) = XCNEWVEC (bitmap, SESE_NUM_VER (region));

  FOR_EACH_BB (bb)
    sese_build_livein_liveouts_bb (region, bb);
}

/* Register basic blocks belonging to a region in a pointer set.  */

static void
register_bb_in_sese (basic_block entry_bb, basic_block exit_bb, sese region)
{
  edge_iterator ei;
  edge e;
  basic_block bb = entry_bb;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!pointer_set_contains (SESE_REGION_BBS (region), e->dest) &&
	  e->dest->index != exit_bb->index)
	{	
	  pointer_set_insert (SESE_REGION_BBS (region), e->dest);
	  register_bb_in_sese (e->dest, exit_bb, region);
	}
    }
}

/* Builds a new SESE region from edges ENTRY and EXIT.  */

sese
new_sese (edge entry, edge exit)
{
  sese res = XNEW (struct sese_d);

  SESE_ENTRY (res) = entry;
  SESE_EXIT (res) = exit;
  SESE_REGION_BBS (res) = pointer_set_create ();
  register_bb_in_sese (entry->dest, exit->dest, res);

  SESE_LIVEOUT (res) = NULL;
  SESE_NUM_VER (res) = 0;
  SESE_LIVEIN (res) = NULL;

  return res;
}

/* Deletes REGION.  */

void
free_sese (sese region)
{
  int i;

  for (i = 0; i < SESE_NUM_VER (region); i++)
    BITMAP_FREE (SESE_LIVEIN_VER (region, i));

  if (SESE_LIVEIN (region))
    free (SESE_LIVEIN (region));

  if (SESE_LIVEOUT (region))
    BITMAP_FREE (SESE_LIVEOUT (region));

  pointer_set_destroy (SESE_REGION_BBS (region));
  XDELETE (region);
}



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

/* Returns true if stack ENTRY is a constant.  */

static bool
iv_stack_entry_is_constant (iv_stack_entry *entry)
{
  return entry->kind == iv_stack_entry_const;
}

/* Returns true if stack ENTRY is an induction variable.  */

static bool
iv_stack_entry_is_iv (iv_stack_entry *entry)
{
  return entry->kind == iv_stack_entry_iv;
}

/* Push (IV, NAME) on STACK.  */

static void 
loop_iv_stack_push_iv (loop_iv_stack stack, tree iv, const char *name)
{
  iv_stack_entry *entry = XNEW (iv_stack_entry);
  name_tree named_iv = XNEW (struct name_tree_d);

  named_iv->t = iv;
  named_iv->name = name;

  entry->kind = iv_stack_entry_iv;
  entry->data.iv = named_iv;

  VEC_safe_push (iv_stack_entry_p, heap, *stack, entry);
}

/* Inserts a CONSTANT in STACK at INDEX.  */

static void
loop_iv_stack_insert_constant (loop_iv_stack stack, int index,
			       tree constant)
{
  iv_stack_entry *entry = XNEW (iv_stack_entry);
  
  entry->kind = iv_stack_entry_const;
  entry->data.constant = constant;

  VEC_safe_insert (iv_stack_entry_p, heap, *stack, index, entry);
}

/* Pops and frees an element out of STACK.  */

static void
loop_iv_stack_pop (loop_iv_stack stack)
{
  iv_stack_entry_p entry = VEC_pop (iv_stack_entry_p, *stack);

  free (entry->data.iv);
  free (entry);
}

/* Get the IV at INDEX in STACK.  */

static tree
loop_iv_stack_get_iv (loop_iv_stack stack, int index)
{
  iv_stack_entry_p entry = VEC_index (iv_stack_entry_p, *stack, index);
  iv_stack_entry_data data = entry->data;

  return iv_stack_entry_is_iv (entry) ? data.iv->t : data.constant;
}

/* Get the IV from its NAME in STACK.  */

static tree
loop_iv_stack_get_iv_from_name (loop_iv_stack stack, const char* name)
{
  int i;
  iv_stack_entry_p entry;

  for (i = 0; VEC_iterate (iv_stack_entry_p, *stack, i, entry); i++)
    {
      name_tree iv = entry->data.iv;
      if (!strcmp (name, iv->name))
	return iv->t;
    }

  return NULL;
}

/* Prints on stderr the contents of STACK.  */

void
debug_loop_iv_stack (loop_iv_stack stack)
{
  int i;
  iv_stack_entry_p entry;
  bool first = true;

  fprintf (stderr, "(");

  for (i = 0; VEC_iterate (iv_stack_entry_p, *stack, i, entry); i++)
    {
      if (first) 
	first = false;
      else
	fprintf (stderr, " ");

      if (iv_stack_entry_is_iv (entry))
	{
	  name_tree iv = entry->data.iv;
	  fprintf (stderr, "%s:", iv->name);
	  print_generic_expr (stderr, iv->t, 0);
	}
      else 
	{
	  tree constant = entry->data.constant;
	  print_generic_expr (stderr, constant, 0);
	  fprintf (stderr, ":");
	  print_generic_expr (stderr, constant, 0);
	}
    }

  fprintf (stderr, ")\n");
}

/* Frees STACK.  */

static void
free_loop_iv_stack (loop_iv_stack stack)
{
  int i;
  iv_stack_entry_p entry;

  for (i = 0; VEC_iterate (iv_stack_entry_p, *stack, i, entry); i++)
    {
      free (entry->data.iv);
      free (entry);
    }

  VEC_free (iv_stack_entry_p, heap, *stack);
}



/* Structure containing the mapping between the CLooG's induction
   variable and the type of the old induction variable.  */
typedef struct ivtype_map_elt_d
{
  tree type;
  const char *cloog_iv;
} *ivtype_map_elt;

/* Print to stderr the element ELT.  */

static void
debug_ivtype_elt (ivtype_map_elt elt)
{
  fprintf (stderr, "(%s, ", elt->cloog_iv);
  print_generic_expr (stderr, elt->type, 0);
  fprintf (stderr, ")\n");
}

/* Helper function for debug_ivtype_map.  */

static int
debug_ivtype_map_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct ivtype_map_elt_d *entry = (struct ivtype_map_elt_d *) *slot;
  debug_ivtype_elt (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

void
debug_ivtype_map (htab_t map)
{
  htab_traverse (map, debug_ivtype_map_1, NULL);
}

/* Constructs a new SCEV_INFO_STR structure for VAR and INSTANTIATED_BELOW.  */

static inline ivtype_map_elt
new_ivtype_map_elt (const char *cloog_iv, tree type)
{
  ivtype_map_elt res;
  
  res = XNEW (struct ivtype_map_elt_d);
  res->cloog_iv = cloog_iv;
  res->type = type;

  return res;
}

/* Computes a hash function for database element ELT.  */

static hashval_t
ivtype_map_elt_info (const void *elt)
{
  return htab_hash_pointer (((const struct ivtype_map_elt_d *) elt)->cloog_iv);
}

/* Compares database elements E1 and E2.  */

static int
eq_ivtype_map_elts (const void *e1, const void *e2)
{
  const struct ivtype_map_elt_d *elt1 = (const struct ivtype_map_elt_d *) e1;
  const struct ivtype_map_elt_d *elt2 = (const struct ivtype_map_elt_d *) e2;

  return (elt1->cloog_iv == elt2->cloog_iv);
}



/* Given a CLOOG_IV, returns the type that it should have in GCC land.
   If the information is not available, i.e. in the case one of the
   transforms created the loop, just return integer_type_node.  */

static tree
gcc_type_for_cloog_iv (const char *cloog_iv, graphite_bb_p gbb)
{
  struct ivtype_map_elt_d tmp;
  PTR *slot;

  tmp.cloog_iv = cloog_iv;
  slot = htab_find_slot (GBB_CLOOG_IV_TYPES (gbb), &tmp, NO_INSERT);

  if (slot && *slot)
    return ((ivtype_map_elt) *slot)->type;

  return integer_type_node;
}

/* Inserts constants derived from the USER_STMT argument list into the
   STACK.  This is needed to map old ivs to constants when loops have
   been eliminated.  */

static void 
loop_iv_stack_patch_for_consts (loop_iv_stack stack,
				struct clast_user_stmt *user_stmt)
{
  struct clast_stmt *t;
  int index = 0;
  CloogStatement *cs = user_stmt->statement;
  graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);

  for (t = user_stmt->substitutions; t; t = t->next) 
    {
      struct clast_expr *expr = (struct clast_expr *) 
	((struct clast_assignment *)t)->RHS;
      struct clast_term *term = (struct clast_term *) expr;

      /* FIXME: What should be done with expr_bin, expr_red?  */
      if (expr->type == expr_term
	  && !term->var)
	{
	  loop_p loop = gbb_loop_at_index (gbb, index);
	  tree oldiv = oldiv_for_loop (GBB_SCOP (gbb), loop);
	  tree type = oldiv ? TREE_TYPE (oldiv) : integer_type_node;
	  tree value = gmp_cst_to_tree (type, term->val);
	  loop_iv_stack_insert_constant (stack, index, value);
	}
      index = index + 1;
    }
}

/* Removes all constants in the iv STACK.  */

static void
loop_iv_stack_remove_constants (loop_iv_stack stack)
{
  int i;
  iv_stack_entry *entry;
  
  for (i = 0; VEC_iterate (iv_stack_entry_p, *stack, i, entry);)
    {
      if (iv_stack_entry_is_constant (entry))
	{
	  free (VEC_index (iv_stack_entry_p, *stack, i));
	  VEC_ordered_remove (iv_stack_entry_p, *stack, i);
	}
      else
	i++;
    }
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
      cloog_matrix_print (file, GBB_DOMAIN (gb));
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
      dump_gbb_conditions (file, gb);
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

      if (bb_in_sese_p (bb, SCOP_REGION (scop))) 
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
	if (bb_in_sese_p (bb, SCOP_REGION (scop))
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
        
	    if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
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

	    if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
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

/* Returns the outermost loop in SCOP that contains BB.  */

static struct loop *
outermost_loop_in_scop (scop_p scop, basic_block bb)
{
  struct loop *nest;

  nest = bb->loop_father;
  while (loop_outer (nest)
	 && loop_in_sese_p (loop_outer (nest), SCOP_REGION (scop)))
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

/* Return true if REF or any of its subtrees contains a
   component_ref.  */

static bool
contains_component_ref_p (tree ref)
{
  if (!ref)
    return false;

  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == COMPONENT_REF)
	return true;

      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Return true if the operand OP is simple.  */

static bool
is_simple_operand (loop_p loop, gimple stmt, tree op) 
{
  /* It is not a simple operand when it is a declaration,  */
  if (DECL_P (op)
      /* or a structure,  */
      || AGGREGATE_TYPE_P (TREE_TYPE (op))
      /* or a COMPONENT_REF,  */
      || contains_component_ref_p (op)
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

	if (lhs && !is_simple_operand (loop, stmt, lhs))
	  return false;

	for (i = 0; i < n; i++)
	  if (!is_simple_operand (loop, stmt, gimple_call_arg (stmt, i)))
	    return false;

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
  gimple stmt;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!stmt_simple_for_scop_p (scop_entry, gsi_stmt (gsi)))
      return gsi_stmt (gsi);

  stmt = last_stmt (bb);
  if (stmt && gimple_code (stmt) == GIMPLE_COND)
    {
      tree lhs = gimple_cond_lhs (stmt);
      tree rhs = gimple_cond_rhs (stmt);

      if (TREE_CODE (TREE_TYPE (lhs)) == REAL_TYPE
	  || TREE_CODE (TREE_TYPE (rhs)) == REAL_TYPE)
	return stmt;
    }

  return NULL;
}

/* Returns true when BB will be represented in graphite.  Return false
   for the basic blocks that contain code eliminated in the code
   generation pass: i.e. induction variables and exit conditions.  */

static bool
graphite_stmt_p (scop_p scop, basic_block bb,
		 VEC (data_reference_p, heap) *drs)
{
  gimple_stmt_iterator gsi;
  loop_p loop = bb->loop_father;

  if (VEC_length (data_reference_p, drs) > 0)
    return true;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
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

	case GIMPLE_ASSIGN:
	  {
	    tree var = gimple_assign_lhs (stmt);
	    var = analyze_scalar_evolution (loop, var);
	    var = instantiate_scev (block_before_scop (scop), loop, var);

	    if (chrec_contains_undetermined (var))
	      return true;

	    break;
	  }

	default:
	  return true;
        }
    }

  return false;
}

/* Store the GRAPHITE representation of BB.  */

static void
new_graphite_bb (scop_p scop, basic_block bb)
{
  struct graphite_bb *gbb;
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 5);
  struct loop *nest = outermost_loop_in_scop (scop, bb);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    find_data_references_in_stmt (nest, gsi_stmt (gsi), &drs);

  if (!graphite_stmt_p (scop, bb, drs))
    {
      free_data_refs (drs);
      return;
    }

  gbb = XNEW (struct graphite_bb);
  bb->aux = gbb;
  GBB_BB (gbb) = bb;
  GBB_SCOP (gbb) = scop;
  GBB_DATA_REFS (gbb) = drs;
  GBB_DOMAIN (gbb) = NULL;
  GBB_CONDITIONS (gbb) = NULL;
  GBB_CONDITION_CASES (gbb) = NULL;
  GBB_LOOPS (gbb) = NULL;
  GBB_STATIC_SCHEDULE (gbb) = NULL;
  GBB_CLOOG_IV_TYPES (gbb) = NULL;
  VEC_safe_push (graphite_bb_p, heap, SCOP_BBS (scop), gbb);
}

/* Frees GBB.  */

static void
free_graphite_bb (struct graphite_bb *gbb)
{
  if (GBB_DOMAIN (gbb))
    cloog_matrix_free (GBB_DOMAIN (gbb));

  if (GBB_CLOOG_IV_TYPES (gbb))
    htab_delete (GBB_CLOOG_IV_TYPES (gbb));

  /* FIXME: free_data_refs is disabled for the moment, but should be
     enabled.

     free_data_refs (GBB_DATA_REFS (gbb)); */

  VEC_free (gimple, heap, GBB_CONDITIONS (gbb));
  VEC_free (gimple, heap, GBB_CONDITION_CASES (gbb));
  VEC_free (loop_p, heap, GBB_LOOPS (gbb));
  GBB_BB (gbb)->aux = 0;
  XDELETE (gbb);
}



/* Structure containing the mapping between the old names and the new
   names used after block copy in the new loop context.  */
typedef struct rename_map_elt_d
{
  tree old_name, new_name;
} *rename_map_elt;


/* Print to stderr the element ELT.  */

static void
debug_rename_elt (rename_map_elt elt)
{
  fprintf (stderr, "(");
  print_generic_expr (stderr, elt->old_name, 0);
  fprintf (stderr, ", ");
  print_generic_expr (stderr, elt->new_name, 0);
  fprintf (stderr, ")\n");
}

/* Helper function for debug_rename_map.  */

static int
debug_rename_map_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct rename_map_elt_d *entry = (struct rename_map_elt_d *) *slot;
  debug_rename_elt (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

void
debug_rename_map (htab_t map)
{
  htab_traverse (map, debug_rename_map_1, NULL);
}

/* Constructs a new SCEV_INFO_STR structure for VAR and INSTANTIATED_BELOW.  */

static inline rename_map_elt
new_rename_map_elt (tree old_name, tree new_name)
{
  rename_map_elt res;
  
  res = XNEW (struct rename_map_elt_d);
  res->old_name = old_name;
  res->new_name = new_name;

  return res;
}

/* Computes a hash function for database element ELT.  */

static hashval_t
rename_map_elt_info (const void *elt)
{
  return htab_hash_pointer (((const struct rename_map_elt_d *) elt)->old_name);
}

/* Compares database elements E1 and E2.  */

static int
eq_rename_map_elts (const void *e1, const void *e2)
{
  const struct rename_map_elt_d *elt1 = (const struct rename_map_elt_d *) e1;
  const struct rename_map_elt_d *elt2 = (const struct rename_map_elt_d *) e2;

  return (elt1->old_name == elt2->old_name);
}

/* Returns the new name associated to OLD_NAME in MAP.  */

static tree
get_new_name_from_old_name (htab_t map, tree old_name)
{
  struct rename_map_elt_d tmp;
  PTR *slot;

  tmp.old_name = old_name;
  slot = htab_find_slot (map, &tmp, NO_INSERT);

  if (slot && *slot)
    return ((rename_map_elt) *slot)->new_name;

  return old_name;
}



/* Creates a new scop starting with ENTRY.  */

static scop_p
new_scop (edge entry, edge exit)
{
  scop_p scop = XNEW (struct scop);

  gcc_assert (entry && exit);

  SCOP_REGION (scop) = new_sese (entry, exit);
  SCOP_BBS (scop) = VEC_alloc (graphite_bb_p, heap, 3);
  SCOP_OLDIVS (scop) = VEC_alloc (name_tree, heap, 3);
  SCOP_LOOPS (scop) = BITMAP_ALLOC (NULL);
  SCOP_LOOP_NEST (scop) = VEC_alloc (loop_p, heap, 3);
  SCOP_ADD_PARAMS (scop) = true;
  SCOP_PARAMS (scop) = VEC_alloc (name_tree, heap, 3);
  SCOP_PROG (scop) = cloog_program_malloc ();
  cloog_program_set_names (SCOP_PROG (scop), cloog_names_malloc ());
  SCOP_LOOP2CLOOG_LOOP (scop) = htab_create (10, hash_loop_to_cloog_loop,
					     eq_loop_to_cloog_loop,
					     free);
  SCOP_LIVEOUT_RENAMES (scop) = htab_create (10, rename_map_elt_info,
					     eq_rename_map_elts, free);
  return scop;
}

/* Deletes SCOP.  */

static void
free_scop (scop_p scop)
{
  int i;
  name_tree p;
  struct graphite_bb *gb;
  name_tree iv;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    free_graphite_bb (gb);

  VEC_free (graphite_bb_p, heap, SCOP_BBS (scop));
  BITMAP_FREE (SCOP_LOOPS (scop));
  VEC_free (loop_p, heap, SCOP_LOOP_NEST (scop));

  for (i = 0; VEC_iterate (name_tree, SCOP_OLDIVS (scop), i, iv); i++)
    free (iv);
  VEC_free (name_tree, heap, SCOP_OLDIVS (scop));
  
  for (i = 0; VEC_iterate (name_tree, SCOP_PARAMS (scop), i, p); i++)
    free (p);

  VEC_free (name_tree, heap, SCOP_PARAMS (scop));
  cloog_program_free (SCOP_PROG (scop));
  htab_delete (SCOP_LOOP2CLOOG_LOOP (scop)); 
  htab_delete (SCOP_LIVEOUT_RENAMES (scop));
  free_sese (SCOP_REGION (scop));
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

/* Return true when it is not possible to represent the upper bound of
   LOOP in the polyhedral representation.  */

static bool
graphite_cannot_represent_loop_niter (loop_p loop)
{
  tree niter = number_of_latch_executions (loop);

  return chrec_contains_undetermined (niter)
    || !scev_is_linear_expression (niter);
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

      /* Mark bbs terminating a SESE region difficult, if they start
	 a condition.  */
      if (VEC_length (edge, bb->succs) > 1)
	result.difficult = true; 

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

        if (graphite_cannot_represent_loop_niter (loop))
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
        /* XXX: For now we just do not join loops with multiple exits.  If the 
           exits lead to the same bb it may be possible to join the loop.  */
        VEC (sd_region, heap) *tmp_scops = VEC_alloc (sd_region, heap, 3);
        VEC (edge, heap) *exits = get_loop_exit_edges (loop);
        edge e;
        int i;
        build_scops_1 (bb, &tmp_scops, loop);

	/* Scan the code dominated by this loop.  This means all bbs, that are
	   are dominated by a bb in this loop, but are not part of this loop.
	   
	   The easiest case:
	     - The loop exit destination is dominated by the exit sources.  
	 
	   TODO: We miss here the more complex cases:
		  - The exit destinations are dominated by another bb inside the
		    loop.
		  - The loop dominates bbs, that are not exit destinations.  */
        for (i = 0; VEC_iterate (edge, exits, i, e); i++)
          if (e->src->loop_father == loop
	      && dominated_by_p (CDI_DOMINATORS, e->dest, e->src))
	    {
	      /* Pass loop_outer to recognize e->dest as loop header in
		 build_scops_1.  */
	      if (e->dest->loop_father->header == e->dest)
		build_scops_1 (e->dest, &tmp_scops,
			       loop_outer (e->dest->loop_father));
	      else
		build_scops_1 (e->dest, &tmp_scops, e->dest->loop_father);
	    }

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
	    if (loop_depth (find_common_loop (loop, dom_bb->loop_father))
		< loop_depth (loop))
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
  open_scop.exit = NULL;
  sinfo.last = NULL;

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

/* Free and compute again all the dominators information.  */

static inline void
recompute_all_dominators (void)
{
  mark_irreducible_loops ();
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
}

/* Verifies properties that GRAPHITE should maintain during translation.  */

static inline void
graphite_verify (void)
{
#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_dominators (CDI_DOMINATORS);
  verify_dominators (CDI_POST_DOMINATORS);
  verify_ssa (false);
  verify_loop_closed_ssa ();
#endif
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

  fix_loop_structure (NULL);

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
  VEC_free (sd_region, heap, tmp_scops);
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

/* Returns the number of reduction phi nodes in LOOP.  */

static int
nb_reductions_in_loop (loop_p loop)
{
  int res = 0;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree scev;
      affine_iv iv;

      if (!is_gimple_reg (PHI_RESULT (phi)))
	continue;

      scev = analyze_scalar_evolution (loop, PHI_RESULT (phi));
      scev = instantiate_parameters (loop, scev);
      if (!simple_iv (loop, loop, PHI_RESULT (phi), &iv, true))
	res++;
    }

  return res;
}

/* A LOOP is in normal form when it contains only one scalar phi node
   that defines the main induction variable of the loop, only one
   increment of the IV, and only one exit condition. */

static tree
graphite_loop_normal_form (loop_p loop)
{
  struct tree_niter_desc niter;
  tree nit;
  gimple_seq stmts;
  edge exit = single_dom_exit (loop);
  bool known_niter = number_of_iterations_exit (loop, exit, &niter, false);

  gcc_assert (known_niter);

  nit = force_gimple_operand (unshare_expr (niter.niter), &stmts, true,
			      NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  /* One IV per loop.  */
  if (nb_reductions_in_loop (loop) > 0)
    return NULL_TREE;

  return canonicalize_loop_ivs (loop, NULL, &nit);
}

/* Record LOOP as occuring in SCOP.  Returns true when the operation
   was successful.  */

static bool
scop_record_loop (scop_p scop, loop_p loop)
{
  tree induction_var;
  name_tree oldiv;

  if (bitmap_bit_p (SCOP_LOOPS (scop), loop->num))
    return true;

  bitmap_set_bit (SCOP_LOOPS (scop), loop->num);
  VEC_safe_push (loop_p, heap, SCOP_LOOP_NEST (scop), loop);

  induction_var = graphite_loop_normal_form (loop);
  if (!induction_var)
    return false;

  oldiv = XNEW (struct name_tree_d);
  oldiv->t = induction_var;
  oldiv->name = get_name (SSA_NAME_VAR (oldiv->t));
  oldiv->loop = loop;
  VEC_safe_push (name_tree, heap, SCOP_OLDIVS (scop), oldiv);
  return true;
}

/* Build the loop nests contained in SCOP.  Returns true when the
   operation was successful.  */

static bool
build_scop_loop_nests (scop_p scop)
{
  unsigned i;
  basic_block bb;
  struct loop *loop0, *loop1;

  FOR_EACH_BB (bb)
    if (bb_in_sese_p (bb, SCOP_REGION (scop)))
      {
	struct loop *loop = bb->loop_father;

	/* Only add loops if they are completely contained in the SCoP.  */
	if (loop->header == bb
	    && bb_in_sese_p (loop->latch, SCOP_REGION (scop)))
	  {
	    if (!scop_record_loop (scop, loop))
	      return false;
	  }
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

  return true;
}

/* Calculate the number of loops around LOOP in the SCOP.  */

static inline int
nb_loops_around_loop_in_scop (struct loop *l, scop_p scop)
{
  int d = 0;

  for (; loop_in_sese_p (l, SCOP_REGION (scop)); d++, l = loop_outer (l));

  return d;
}

/* Calculate the number of loops around GB in the current SCOP.  */

int
nb_loops_around_gb (graphite_bb_p gb)
{
  return nb_loops_around_loop_in_scop (gbb_loop (gb), GBB_SCOP (gb));
}

/* Returns the dimensionality of an enclosing loop iteration domain
   with respect to enclosing SCoP for a given data reference REF.  The
   returned dimensionality is homogeneous (depth of loop nest + number
   of SCoP parameters + const).  */

int
ref_nb_loops (data_reference_p ref)
{
  loop_p loop = loop_containing_stmt (DR_STMT (ref));
  scop_p scop = DR_SCOP (ref);

  return nb_loops_around_loop_in_scop (loop, scop) + scop_nb_params (scop) + 2;
}

/* Build dynamic schedules for all the BBs. */

static void
build_scop_dynamic_schedules (scop_p scop)
{
  int i, dim, loop_num, row, col;
  graphite_bb_p gb;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      loop_num = GBB_BB (gb)->loop_father->num;

      if (loop_num != 0)
        {
          dim = nb_loops_around_gb (gb);
          GBB_DYNAMIC_SCHEDULE (gb) = cloog_matrix_alloc (dim, dim);

          for (row = 0; row < GBB_DYNAMIC_SCHEDULE (gb)->NbRows; row++)
            for (col = 0; col < GBB_DYNAMIC_SCHEDULE (gb)->NbColumns; col++)
              if (row == col)
                value_set_si (GBB_DYNAMIC_SCHEDULE (gb)->p[row][col], 1);
              else
                value_set_si (GBB_DYNAMIC_SCHEDULE (gb)->p[row][col], 0);
        }
      else
        GBB_DYNAMIC_SCHEDULE (gb) = NULL;
    }
}

/* Returns the number of loops that are identical at the beginning of
   the vectors A and B.  */

static int
compare_prefix_loops (VEC (loop_p, heap) *a, VEC (loop_p, heap) *b)
{
  int i;
  loop_p ea;
  int lb;

  if (!a || !b)
    return 0;

  lb = VEC_length (loop_p, b);

  for (i = 0; VEC_iterate (loop_p, a, i, ea); i++)
    if (i >= lb
	|| ea != VEC_index (loop_p, b, i))
      return i;

  return 0;
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
  int i;
  graphite_bb_p gb;
  int nb_loops = scop_nb_loops (scop);
  lambda_vector static_schedule = lambda_vector_new (nb_loops + 1);
  VEC (loop_p, heap) *loops_previous = NULL;

  /* We have to start schedules at 0 on the first component and
     because we cannot compare_prefix_loops against a previous loop,
     prefix will be equal to zero, and that index will be
     incremented before copying.  */
  static_schedule[0] = -1;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      int prefix = compare_prefix_loops (loops_previous, GBB_LOOPS (gb));
      int nb = gbb_nb_loops (gb);

      loops_previous = GBB_LOOPS (gb);
      memset (&(static_schedule[prefix + 1]), 0, sizeof (int) * (nb_loops - prefix));
      ++static_schedule[prefix];
      GBB_STATIC_SCHEDULE (gb) = lambda_vector_new (nb + 1);
      lambda_vector_copy (static_schedule, 
			  GBB_STATIC_SCHEDULE (gb), nb + 1);
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

  gcc_assert (SCOP_ADD_PARAMS (scop));

  nvar = XNEW (struct name_tree_d);
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
	  if (c)
	    {
	      Value val;
	      gcc_assert (host_integerp (TREE_OPERAND (e, 1), 0));
	      value_init (val);
	      value_set_si (val, int_cst_value (TREE_OPERAND (e, 1)));
	      value_multiply (k, k, val);
	      value_clear (val);
	    }
	  scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
	}
      else
	{
	  if (c)
	    {
	      Value val;
	      gcc_assert (host_integerp (TREE_OPERAND (e, 0), 0));
	      value_init (val);
	      value_set_si (val, int_cst_value (TREE_OPERAND (e, 0)));
	      value_multiply (k, k, val);
	      value_clear (val);
	    }
	  scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, subtract);
	}
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, subtract);
      break;

    case MINUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, subtract);
      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, r, k, !subtract);
      break;

    case NEGATE_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, r, k, !subtract);
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

    CASE_CONVERT:
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
find_params_in_bb (scop_p scop, graphite_bb_p gb)
{
  int i;
  data_reference_p dr;
  gimple stmt;
  loop_p father = GBB_BB (gb)->loop_father;

  for (i = 0; VEC_iterate (data_reference_p, GBB_DATA_REFS (gb), i, dr); i++)
    {
      struct irp_data irp;

      irp.loop = father;
      irp.scop = scop;
      for_each_index (&dr->ref, idx_record_params, &irp);
    }

  /* Find parameters in conditional statements.  */ 
  for (i = 0; VEC_iterate (gimple, GBB_CONDITIONS (gb), i, stmt); i++)
    {
      Value one;
      loop_p loop = father;

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
    find_params_in_bb (scop, gb);

  SCOP_ADD_PARAMS (scop) = false;
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

/* Builds the constraint matrix for LOOP in SCOP.  NB_OUTER_LOOPS is the
   number of loops surrounding LOOP in SCOP.  OUTER_CSTR gives the
   constraints matrix for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              CloogMatrix *outer_cstr, int nb_outer_loops)
{
  int i, j, row;
  CloogMatrix *cstr;
  graphite_bb_p gb;

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

  if (loop->inner && loop_in_sese_p (loop->inner, SCOP_REGION (scop)))
    build_loop_iteration_domains (scop, loop->inner, cstr, nb_outer_loops + 1);

  /* Only go to the next loops, if we are not at the outermost layer.  These
     have to be handled seperately, as we can be sure, that the chain at this
     layer will be connected.  */
  if (nb_outer_loops != 0 && loop->next && loop_in_sese_p (loop->next,
							   SCOP_REGION (scop)))
    build_loop_iteration_domains (scop, loop->next, outer_cstr, nb_outer_loops);

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    if (gbb_loop (gb) == loop)
      GBB_DOMAIN (gb) = cloog_matrix_copy (cstr);

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
      nb_cols = nb_loops_around_gb (gb) + scop_nb_params (scop) + 2;
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
        case GIMPLE_SWITCH:
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

    if (domain)
      {
	for (i = 0; i < nb_rows; i++)
	  for (j = 0; j < nb_cols; j++)
	    value_assign (new_domain->p[i][j], domain->p[i][j]);

	cloog_matrix_free (domain);
      }

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

/* Returns true when PHI defines an induction variable in the loop
   containing the PHI node.  */

static bool
phi_node_is_iv (gimple phi)
{
  loop_p loop = gimple_bb (phi)->loop_father;
  tree scev = analyze_scalar_evolution (loop, gimple_phi_result (phi));

  return tree_contains_chrecs (scev, NULL);
}

/* Returns true when BB contains scalar phi nodes that are not an
   induction variable of a loop.  */

static bool
bb_contains_non_iv_scalar_phi_nodes (basic_block bb)
{
  gimple phi = NULL;
  gimple_stmt_iterator si;

  for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
    if (is_gimple_reg (gimple_phi_result (gsi_stmt (si))))
      {
	/* Store the unique scalar PHI node: at this point, loops
	   should be in cannonical form, so we expect to see at most
	   one scalar phi node in the loop header.  */
	if (phi
	    || bb != bb->loop_father->header)
	  return true;

	phi = gsi_stmt (si);
      }

  if (!phi
      || phi_node_is_iv (phi))
    return false;

  return true;
}

/* Helper recursive function.  Record in CONDITIONS and CASES all
   conditions from 'if's and 'switch'es occurring in BB from SCOP.

   Returns false when the conditions contain scalar computations that
   depend on the condition, i.e. when there are scalar phi nodes on
   the junction after the condition.  Only the computations occurring
   on memory can be handled in the polyhedral model: operations that
   define scalar evolutions in conditions, that can potentially be
   used to index memory, can't be handled by the polyhedral model.  */

static bool
build_scop_conditions_1 (VEC (gimple, heap) **conditions,
			 VEC (gimple, heap) **cases, basic_block bb,
			 scop_p scop)
{
  bool res = true;
  int i, j;
  graphite_bb_p gbb;
  basic_block bb_child, bb_iter;
  VEC (basic_block, heap) *dom;
  gimple stmt;
  
  /* Make sure we are in the SCoP.  */
  if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
    return true;

  if (bb_contains_non_iv_scalar_phi_nodes (bb))
    return false;

  gbb = gbb_from_bb (bb);
  if (gbb)
    {
      GBB_CONDITIONS (gbb) = VEC_copy (gimple, heap, *conditions);
      GBB_CONDITION_CASES (gbb) = VEC_copy (gimple, heap, *cases);
    }

  dom = get_dominated_by (CDI_DOMINATORS, bb);

  stmt = last_stmt (bb);
  if (stmt)
    {
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
		else 
		  {
		    gcc_assert (e->flags & EDGE_FALSE_VALUE);
		    VEC_safe_push (gimple, heap, *cases, NULL);
		  }

		VEC_safe_push (gimple, heap, *conditions, stmt);
		if (!build_scop_conditions_1 (conditions, cases, e->dest, scop))
		  {
		    res = false;
		    goto done;
		  }
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

		for (k = 0; k < n; k++)
		  if (i != k
		      && label_to_block 
		      (CASE_LABEL (gimple_switch_label (stmt, k))) == bb_child)
		    break;

		/* Switches with multiple case values for the same
		   block are not handled.  */
		if (k != n
		    /* Switch cases with more than one predecessor are
		       not handled.  */
		    || VEC_length (edge, bb_child->preds) != 1)
		  {
		    res = false;
		    goto done;
		  }

		/* Recursively scan the corresponding 'case' block.  */
		for (gsi_search_gimple_label = gsi_start_bb (bb_child);
		     !gsi_end_p (gsi_search_gimple_label);
		     gsi_next (&gsi_search_gimple_label))
		  {
		    gimple label = gsi_stmt (gsi_search_gimple_label);

		    if (gimple_code (label) == GIMPLE_LABEL)
		      {
			tree t = gimple_label_label (label);

			gcc_assert (t == gimple_switch_label (stmt, i));
			VEC_replace (gimple, *cases, n_cases, label);
			break;
		      }
		  }

		if (!build_scop_conditions_1 (conditions, cases, bb_child, scop))
		  {
		    res = false;
		    goto done;
		  }

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
    if (!build_scop_conditions_1 (conditions, cases, bb_child, scop))
      {
	res = false;
	goto done;
      }

 done:
  VEC_free (basic_block, heap, dom);
  return res;
}

/* Record all conditions from SCOP.

   Returns false when the conditions contain scalar computations that
   depend on the condition, i.e. when there are scalar phi nodes on
   the junction after the condition.  Only the computations occurring
   on memory can be handled in the polyhedral model: operations that
   define scalar evolutions in conditions, that can potentially be
   used to index memory, can't be handled by the polyhedral model.  */

static bool
build_scop_conditions (scop_p scop)
{
  bool res;
  VEC (gimple, heap) *conditions = NULL;
  VEC (gimple, heap) *cases = NULL;

  res = build_scop_conditions_1 (&conditions, &cases, SCOP_ENTRY (scop), scop);

  VEC_free (gimple, heap, conditions);
  VEC_free (gimple, heap, cases);
  return res;
}

/* Traverses all the GBBs of the SCOP and add their constraints to the
   iteration domains.  */

static void
add_conditions_to_constraints (scop_p scop)
{
  int i;
  graphite_bb_p gbb;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gbb); i++)
    add_conditions_to_domain (gbb);
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
    if (!loop_in_sese_p (loop_outer (loop), SCOP_REGION (scop)))
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
   information for a subscript from AF, relatively to the loop
   indexes from LOOP_NEST and parameter indexes from PARAMS.  NDIM is
   the dimension of the array access, i.e. the number of
   subscripts.  Returns true when the operation succeeds.  */

static bool
build_access_matrix_with_af (tree af, lambda_vector cy,
			     scop_p scop, int ndim)
{
  int param_col;

  switch (TREE_CODE (af))
    {
    case POLYNOMIAL_CHREC:
      {
        struct loop *outer_loop;
	tree left = CHREC_LEFT (af);
	tree right = CHREC_RIGHT (af);
	int var;

	if (TREE_CODE (right) != INTEGER_CST)
	  return false;

        outer_loop = get_loop (CHREC_VARIABLE (af));
        var = nb_loops_around_loop_in_scop (outer_loop, scop);
	cy[var] = int_cst_value (right);

	switch (TREE_CODE (left))
	  {
	  case POLYNOMIAL_CHREC:
	    return build_access_matrix_with_af (left, cy, scop, ndim);

	  case INTEGER_CST:
	    cy[ndim - 1] = int_cst_value (left);
	    return true;

	  default:
	    return build_access_matrix_with_af (left, cy, scop, ndim);
	  }
      }

    case PLUS_EXPR:
      build_access_matrix_with_af (TREE_OPERAND (af, 0), cy, scop, ndim);
      build_access_matrix_with_af (TREE_OPERAND (af, 1), cy, scop, ndim);
      return true;
      
    case MINUS_EXPR:
      build_access_matrix_with_af (TREE_OPERAND (af, 0), cy, scop, ndim);
      build_access_matrix_with_af (TREE_OPERAND (af, 1), cy, scop, ndim);
      return true;

    case INTEGER_CST:
      cy[ndim - 1] = int_cst_value (af);
      return true;

    case SSA_NAME:
      param_col = param_index (af, scop);      
      cy [ndim - scop_nb_params (scop) + param_col - 1] = 1; 
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

  AM_MATRIX (am) = VEC_alloc (lambda_vector, gc, ndim);
  DR_SCOP (ref) = GBB_SCOP (gb);

  for (i = 0; i < ndim; i++)
    {
      lambda_vector v = lambda_vector_new (ref_nb_loops (ref));
      scop_p scop = GBB_SCOP (gb);
      tree af = DR_ACCESS_FN (ref, i);

      if (!build_access_matrix_with_af (af, v, scop, ref_nb_loops (ref)))
	return false;

      VEC_quick_push (lambda_vector, AM_MATRIX (am), v);
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

  /* FIXME: Construction of access matrix is disabled until some
     pass, like the data dependence analysis, is using it.  */
  return;

  for (i = 0; VEC_iterate (graphite_bb_p, SCOP_BBS (scop), i, gb); i++)
    {
      int j;
      data_reference_p dr;

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

  if (params)
    for (i = 0; VEC_iterate (name_tree, params, i, t); i++)
      if (!strcmp (name, t->name))
	return t->t;

  iv = loop_iv_stack_get_iv_from_name (ivstack, name);
  if (iv)
    return iv;

  gcc_unreachable ();
}

/* Returns the maximal precision type for expressions E1 and E2.  */

static inline tree
max_precision_type (tree e1, tree e2)
{
  tree type1 = TREE_TYPE (e1);
  tree type2 = TREE_TYPE (e2);
  return TYPE_PRECISION (type1) > TYPE_PRECISION (type2) ? type1 : type2;
}

static tree
clast_to_gcc_expression (tree, struct clast_expr *, VEC (name_tree, heap) *,
			 loop_iv_stack);

/* Converts a Cloog reduction expression R with reduction operation OP
   to a GCC expression tree of type TYPE.  PARAMS is a vector of
   parameters of the scop, and IVSTACK contains the stack of induction
   variables.  */

static tree
clast_to_gcc_expression_red (tree type, enum tree_code op,
			     struct clast_reduction *r,
			     VEC (name_tree, heap) *params,
			     loop_iv_stack ivstack)
{
  int i;
  tree res = clast_to_gcc_expression (type, r->elts[0], params, ivstack);

  for (i = 1; i < r->n; i++)
    {
      tree t = clast_to_gcc_expression (type, r->elts[i], params, ivstack);
      res = fold_build2 (op, type, res, t);
    }
  return res;
}

/* Converts a Cloog AST expression E back to a GCC expression tree of
   type TYPE.  PARAMS is a vector of parameters of the scop, and
   IVSTACK contains the stack of induction variables.  */

static tree
clast_to_gcc_expression (tree type, struct clast_expr *e,
			 VEC (name_tree, heap) *params,
			 loop_iv_stack ivstack)
{
  switch (e->type)
    {
    case expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  {
	    if (value_one_p (t->val))
	      {
		tree name = clast_name_to_gcc (t->var, params, ivstack);
		return fold_convert (type, name);
	      }

	    else if (value_mone_p (t->val))
	      {
		tree name = clast_name_to_gcc (t->var, params, ivstack);
		name = fold_convert (type, name);
		return fold_build1 (NEGATE_EXPR, type, name);
	      }
	    else
	      {
		tree name = clast_name_to_gcc (t->var, params, ivstack);
		tree cst = gmp_cst_to_tree (type, t->val);
		name = fold_convert (type, name);
		return fold_build2 (MULT_EXPR, type, cst, name);
	      }
	  }
	else
	  return gmp_cst_to_tree (type, t->val);
      }

    case expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;

        switch (r->type)
          {
	  case clast_red_sum:
	    return clast_to_gcc_expression_red (type, PLUS_EXPR, r, params, ivstack);

	  case clast_red_min:
	    return clast_to_gcc_expression_red (type, MIN_EXPR, r, params, ivstack);

	  case clast_red_max:
	    return clast_to_gcc_expression_red (type, MAX_EXPR, r, params, ivstack);

	  default:
	    gcc_unreachable ();
          }
        break;
      }

    case expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	tree tl = clast_to_gcc_expression (type, lhs, params, ivstack);
	tree tr = gmp_cst_to_tree (type, b->RHS);

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

/* Returns the type for the expression E.  */

static tree
gcc_type_for_clast_expr (struct clast_expr *e,
			 VEC (name_tree, heap) *params,
			 loop_iv_stack ivstack)
{
  switch (e->type)
    {
    case expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  return TREE_TYPE (clast_name_to_gcc (t->var, params, ivstack));
	else
	  return NULL_TREE;
      }

    case expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;

	if (r->n == 1)
	  return gcc_type_for_clast_expr (r->elts[0], params, ivstack);
	else 
	  {
	    int i;
	    for (i = 0; i < r->n; i++)
	      {
		tree type = gcc_type_for_clast_expr (r->elts[i], params, ivstack);
		if (type)
		  return type;
	      }
	    return NULL_TREE;
	  }
      }

    case expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	return gcc_type_for_clast_expr (lhs, params, ivstack);
      }

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Returns the type for the equation CLEQ.  */

static tree
gcc_type_for_clast_eq (struct clast_equation *cleq,
		       VEC (name_tree, heap) *params,
		       loop_iv_stack ivstack)
{
  tree type = gcc_type_for_clast_expr (cleq->LHS, params, ivstack);
  if (type)
    return type;

  return gcc_type_for_clast_expr (cleq->RHS, params, ivstack);
}

/* Translates a clast equation CLEQ to a tree.  */

static tree
graphite_translate_clast_equation (scop_p scop,
				   struct clast_equation *cleq,
				   loop_iv_stack ivstack)
{
  enum tree_code comp;
  tree type = gcc_type_for_clast_eq (cleq, SCOP_PARAMS (scop), ivstack);
  tree lhs = clast_to_gcc_expression (type, cleq->LHS, SCOP_PARAMS (scop), ivstack);
  tree rhs = clast_to_gcc_expression (type, cleq->RHS, SCOP_PARAMS (scop), ivstack);

  if (cleq->sign == 0)
    comp = EQ_EXPR;

  else if (cleq->sign > 0)
    comp = GE_EXPR;

  else
    comp = LE_EXPR;

  return fold_build2 (comp, type, lhs, rhs);
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
	cond = fold_build2 (TRUTH_AND_EXPR, TREE_TYPE (eq), cond, eq);
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

/* Walks a CLAST and returns the first statement in the body of a
   loop.  */

static struct clast_user_stmt *
clast_get_body_of_loop (struct clast_stmt *stmt)
{
  if (!stmt
      || CLAST_STMT_IS_A (stmt, stmt_user))
    return (struct clast_user_stmt *) stmt;

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    return clast_get_body_of_loop (((struct clast_for *) stmt)->body);

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    return clast_get_body_of_loop (((struct clast_guard *) stmt)->then);

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    return clast_get_body_of_loop (((struct clast_block *) stmt)->body);

  gcc_unreachable ();
}

/* Returns the induction variable for the loop that gets translated to
   STMT.  */

static tree
gcc_type_for_iv_of_clast_loop (struct clast_for *stmt_for)
{
  struct clast_user_stmt *stmt = clast_get_body_of_loop ((struct clast_stmt *) stmt_for);
  const char *cloog_iv = stmt_for->iterator;
  CloogStatement *cs = stmt->statement;
  graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);

  return gcc_type_for_cloog_iv (cloog_iv, gbb);
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
  tree type = gcc_type_for_iv_of_clast_loop (stmt);
  VEC (name_tree, heap) *params = SCOP_PARAMS (scop);
  tree lb = clast_to_gcc_expression (type, stmt->LB, params, ivstack);
  tree ub = clast_to_gcc_expression (type, stmt->UB, params, ivstack);
  tree stride = gmp_cst_to_tree (type, stmt->stride);
  tree ivvar = create_tmp_var (type, "graphiteIV");
  tree iv_before;
  loop_p loop = create_empty_loop_on_edge
    (entry_edge, lb, stride, ub, ivvar, &iv_before,
     outer ? outer : entry_edge->src->loop_father);

  add_referenced_var (ivvar);
  loop_iv_stack_push_iv (ivstack, iv_before, stmt->iterator);
  return loop;
}

/* Rename the SSA_NAMEs used in STMT and that appear in IVSTACK.  */

static void 
rename_variables_in_stmt (gimple stmt, htab_t map)
{
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
    {
      tree use = USE_FROM_PTR (use_p);
      tree new_name = get_new_name_from_old_name (map, use);

      replace_exp (use_p, new_name);
    }

  update_stmt (stmt);
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

/* Returns true if NAME is an induction variable.  */

static bool
is_iv (tree name)
{
  return gimple_code (SSA_NAME_DEF_STMT (name)) == GIMPLE_PHI;
}

static void expand_scalar_variables_stmt (gimple, basic_block, scop_p,
					  htab_t);
static tree
expand_scalar_variables_expr (tree, tree, enum tree_code, tree, basic_block,
			      scop_p, htab_t, gimple_stmt_iterator *);

/* Copies at GSI all the scalar computations on which the ssa_name OP0
   depends on in the SCOP: these are all the scalar variables used in
   the definition of OP0, that are defined outside BB and still in the
   SCOP, i.e. not a parameter of the SCOP.  The expression that is
   returned contains only induction variables from the generated code:
   MAP contains the induction variables renaming mapping, and is used
   to translate the names of induction variables.  */

static tree
expand_scalar_variables_ssa_name (tree op0, basic_block bb,
				  scop_p scop, htab_t map, 
				  gimple_stmt_iterator *gsi)
{
  tree var0, var1, type;
  gimple def_stmt;
  enum tree_code subcode;
      
  if (is_parameter (scop, op0)
      || is_iv (op0))
    return get_new_name_from_old_name (map, op0);
      
  def_stmt = SSA_NAME_DEF_STMT (op0);
      
  if (gimple_bb (def_stmt) == bb)
    {
      /* If the defining statement is in the basic block already
	 we do not need to create a new expression for it, we
	 only need to ensure its operands are expanded.  */
      expand_scalar_variables_stmt (def_stmt, bb, scop, map);
      return get_new_name_from_old_name (map, op0);
    }
  else
    {
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN
	  || !bb_in_sese_p (gimple_bb (def_stmt), SCOP_REGION (scop)))
	return get_new_name_from_old_name (map, op0);

      var0 = gimple_assign_rhs1 (def_stmt);
      subcode = gimple_assign_rhs_code (def_stmt);
      var1 = gimple_assign_rhs2 (def_stmt);
      type = gimple_expr_type (def_stmt);

      return expand_scalar_variables_expr (type, var0, subcode, var1, bb, scop,
					   map, gsi);
    }
}

/* Copies at GSI all the scalar computations on which the expression
   OP0 CODE OP1 depends on in the SCOP: these are all the scalar
   variables used in OP0 and OP1, defined outside BB and still defined
   in the SCOP, i.e. not a parameter of the SCOP.  The expression that
   is returned contains only induction variables from the generated
   code: MAP contains the induction variables renaming mapping, and is
   used to translate the names of induction variables.  */

static tree
expand_scalar_variables_expr (tree type, tree op0, enum tree_code code, 
			      tree op1, basic_block bb, scop_p scop, 
			      htab_t map, gimple_stmt_iterator *gsi)
{
  if (TREE_CODE_CLASS (code) == tcc_constant
      || TREE_CODE_CLASS (code) == tcc_declaration)
    return op0;

  /* For data references we have to duplicate also its memory
     indexing.  */
  if (TREE_CODE_CLASS (code) == tcc_reference)
    {
      switch (code)
	{
	case INDIRECT_REF:
	  {
	    tree old_name = TREE_OPERAND (op0, 0);
	    tree expr = expand_scalar_variables_ssa_name
	      (old_name, bb, scop, map, gsi);
	    tree new_name = force_gimple_operand_gsi (gsi, expr, true, NULL,
						      true, GSI_SAME_STMT);

	    return fold_build1 (code, type, new_name);
	  }

	case ARRAY_REF:
	  {
	    tree op00 = TREE_OPERAND (op0, 0);
	    tree op01 = TREE_OPERAND (op0, 1);
	    tree op02 = TREE_OPERAND (op0, 2);
	    tree op03 = TREE_OPERAND (op0, 3);
	    tree base = expand_scalar_variables_expr
	      (TREE_TYPE (op00), op00, TREE_CODE (op00), NULL, bb, scop,
	       map, gsi);
	    tree subscript = expand_scalar_variables_expr
	      (TREE_TYPE (op01), op01, TREE_CODE (op01), NULL, bb, scop,
	       map, gsi);

	    return build4 (ARRAY_REF, type, base, subscript, op02, op03);
	  }

	default:
	  /* The above cases should catch everything.  */
	  gcc_unreachable ();
	}
    }

  if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = expand_scalar_variables_expr (op0_type, op0, op0_code,
						    NULL, bb, scop, map, gsi);
  
      return fold_build1 (code, type, op0_expr);
    }

  if (TREE_CODE_CLASS (code) == tcc_binary)
    {
      tree op0_type = TREE_TYPE (op0);
      enum tree_code op0_code = TREE_CODE (op0);
      tree op0_expr = expand_scalar_variables_expr (op0_type, op0, op0_code,
						    NULL, bb, scop, map, gsi);
      tree op1_type = TREE_TYPE (op1);
      enum tree_code op1_code = TREE_CODE (op1);
      tree op1_expr = expand_scalar_variables_expr (op1_type, op1, op1_code,
						    NULL, bb, scop, map, gsi);

      return fold_build2 (code, type, op0_expr, op1_expr);
    }

  if (code == SSA_NAME)
    return expand_scalar_variables_ssa_name (op0, bb, scop, map, gsi);

  gcc_unreachable ();
  return NULL;
}

/* Copies at the beginning of BB all the scalar computations on which
   STMT depends on in the SCOP: these are all the scalar variables used
   in STMT, defined outside BB and still defined in the SCOP, i.e. not a
   parameter of the SCOP.  The expression that is returned contains
   only induction variables from the generated code: MAP contains the
   induction variables renaming mapping, and is used to translate the
   names of induction variables.  */
 
static void
expand_scalar_variables_stmt (gimple stmt, basic_block bb, scop_p scop,
			      htab_t map)
{
  ssa_op_iter iter;
  use_operand_p use_p;
  gimple_stmt_iterator gsi = gsi_after_labels (bb);

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      tree type = TREE_TYPE (use);
      enum tree_code code = TREE_CODE (use);
      tree use_expr = expand_scalar_variables_expr (type, use, code, NULL, bb,
						    scop, map, &gsi);
      if (use_expr != use)
	{
	  tree new_use =
	    force_gimple_operand_gsi (&gsi, use_expr, true, NULL,
				      true, GSI_NEW_STMT);
	  replace_exp (use_p, new_use);
	}
    }

  update_stmt (stmt);
}

/* Copies at the beginning of BB all the scalar computations on which
   BB depends on in the SCOP: these are all the scalar variables used
   in BB, defined outside BB and still defined in the SCOP, i.e. not a
   parameter of the SCOP.  The expression that is returned contains
   only induction variables from the generated code: MAP contains the
   induction variables renaming mapping, and is used to translate the
   names of induction variables.  */

static void 
expand_scalar_variables (basic_block bb, scop_p scop, htab_t map)
{
  gimple_stmt_iterator gsi;
  
  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);
      expand_scalar_variables_stmt (stmt, bb, scop, map);
      gsi_next (&gsi);
    }
}

/* Rename all the SSA_NAMEs from block BB according to the MAP.  */

static void 
rename_variables (basic_block bb, htab_t map)
{
  gimple_stmt_iterator gsi;
  
  for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    rename_variables_in_stmt (gsi_stmt (gsi), map);
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

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag cleared.  */

static edge
get_false_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_TRUE_VALUE)) 
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Inserts in MAP a tuple (OLD_NAME, NEW_NAME) for the induction
   variables of the loops around GBB in SCOP, i.e. GBB_LOOPS.
   NEW_NAME is obtained from IVSTACK.  IVSTACK has the same stack
   ordering as GBB_LOOPS.  */

static void
build_iv_mapping (loop_iv_stack ivstack, htab_t map, gbb_p gbb, scop_p scop)
{
  int i;
  name_tree iv;
  PTR *slot;

  for (i = 0; VEC_iterate (name_tree, SCOP_OLDIVS (scop), i, iv); i++)
    {
      struct rename_map_elt_d tmp;

      if (!flow_bb_inside_loop_p (iv->loop, GBB_BB (gbb)))
	continue;

      tmp.old_name = iv->t;
      slot = htab_find_slot (map, &tmp, INSERT);

      if (!*slot)
	{
	  tree new_name = loop_iv_stack_get_iv (ivstack, 
						gbb_loop_index (gbb, iv->loop));
	  *slot = new_rename_map_elt (iv->t, new_name);
	}
    }
}

/* Register in MAP the tuple (old_name, new_name).  */

static void
register_old_and_new_names (htab_t map, tree old_name, tree new_name)
{
  struct rename_map_elt_d tmp;
  PTR *slot;

  tmp.old_name = old_name;
  slot = htab_find_slot (map, &tmp, INSERT);

  if (!*slot)
    *slot = new_rename_map_elt (old_name, new_name);
}

/* Create a duplicate of the basic block BB.  NOTE: This does not
   preserve SSA form.  */

static void
graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb, htab_t map)
{
  gimple_stmt_iterator gsi, gsi_tgt;

  gsi_tgt = gsi_start_bb (new_bb);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      def_operand_p def_p;
      ssa_op_iter op_iter;
      int region;
      gimple stmt = gsi_stmt (gsi);
      gimple copy;

      if (gimple_code (stmt) == GIMPLE_LABEL)
	continue;

      /* Create a new copy of STMT and duplicate STMT's virtual
	 operands.  */
      copy = gimple_copy (stmt);
      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);
      mark_sym_for_renaming (gimple_vop (cfun));

      region = lookup_stmt_eh_region (stmt);
      if (region >= 0)
	add_stmt_to_eh_region (copy, region);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* Create new names for all the definitions created by COPY and
	 add replacement mappings for each new name.  */
      FOR_EACH_SSA_DEF_OPERAND (def_p, copy, op_iter, SSA_OP_ALL_DEFS)
	{
	  tree old_name = DEF_FROM_PTR (def_p);
	  tree new_name = create_new_def_for (old_name, copy, def_p);
	  register_old_and_new_names (map, old_name, new_name);
	}
    }
}

/* Records in SCOP_LIVEOUT_RENAMES the names that are live out of
   the SCOP and that appear in the RENAME_MAP.  */

static void
register_scop_liveout_renames (scop_p scop, htab_t rename_map)
{
  int i;
  sese region = SCOP_REGION (scop);

  for (i = 0; i < SESE_NUM_VER (region); i++)
    if (bitmap_bit_p (SESE_LIVEOUT (region), i)
	&& is_gimple_reg (ssa_name (i)))
      {
	tree old_name = ssa_name (i);
	tree new_name = get_new_name_from_old_name (rename_map, old_name);

	register_old_and_new_names (SCOP_LIVEOUT_RENAMES (scop),
				    old_name, new_name);
      }
}

/* Copies BB and includes in the copied BB all the statements that can
   be reached following the use-def chains from the memory accesses,
   and returns the next edge following this new block.  */
 
static edge
copy_bb_and_scalar_dependences (basic_block bb, scop_p scop,
				edge next_e, htab_t map)
{
  basic_block new_bb = split_edge (next_e);

  next_e = single_succ_edge (new_bb);
  graphite_copy_stmts_from_block (bb, new_bb, map);
  remove_condition (new_bb);
  rename_variables (new_bb, map);
  remove_phi_nodes (new_bb);
  expand_scalar_variables (new_bb, scop, map);
  register_scop_liveout_renames (scop, map);

  return next_e;
}

/* Helper function for htab_traverse in insert_loop_close_phis.  */

static int
add_loop_exit_phis (void **slot, void *s)
{
  struct rename_map_elt_d *entry = (struct rename_map_elt_d *) *slot;
  tree new_name = entry->new_name;
  basic_block bb = (basic_block) s;
  gimple phi = create_phi_node (new_name, bb);
  tree res = create_new_def_for (gimple_phi_result (phi), phi,
				 gimple_phi_result_ptr (phi));

  add_phi_arg (phi, new_name, single_pred_edge (bb));

  entry->new_name = res;
  *slot = entry;
  return 1;
}

/* Iterate over the SCOP_LIVEOUT_RENAMES (SCOP) and get tuples of the
   form (OLD_NAME, NEW_NAME).  Insert in BB "RES = phi (NEW_NAME)",
   and finally register in SCOP_LIVEOUT_RENAMES (scop) the tuple
   (OLD_NAME, RES).  */

static void
insert_loop_close_phis (scop_p scop, basic_block bb)
{
  update_ssa (TODO_update_ssa);
  htab_traverse (SCOP_LIVEOUT_RENAMES (scop), add_loop_exit_phis, bb);
  update_ssa (TODO_update_ssa);
}

/* Helper structure for htab_traverse in insert_guard_phis.  */

struct igp {
  basic_block bb;
  edge true_edge, false_edge;
  htab_t liveout_before_guard;
};

/* Return the default name that is before the guard.  */

static tree
default_liveout_before_guard (htab_t liveout_before_guard, tree old_name)
{
  tree res = get_new_name_from_old_name (liveout_before_guard, old_name);

  if (res == old_name)
    {
      if (is_gimple_reg (res))
	return fold_convert (TREE_TYPE (res), integer_zero_node);
      return gimple_default_def (cfun, res);
    }

  return res;
}

/* Helper function for htab_traverse in insert_guard_phis.  */

static int
add_guard_exit_phis (void **slot, void *s)
{
  struct rename_map_elt_d *entry = (struct rename_map_elt_d *) *slot;
  struct igp *i = (struct igp *) s;
  basic_block bb = i->bb;
  edge true_edge = i->true_edge;
  edge false_edge = i->false_edge;
  tree name1 = entry->new_name;
  tree name2 = default_liveout_before_guard (i->liveout_before_guard,
					     entry->old_name);
  gimple phi = create_phi_node (name1, bb);
  tree res = create_new_def_for (gimple_phi_result (phi), phi,
				 gimple_phi_result_ptr (phi));

  add_phi_arg (phi, name1, true_edge);
  add_phi_arg (phi, name2, false_edge);

  entry->new_name = res;
  *slot = entry;
  return 1;
}

/* Iterate over the SCOP_LIVEOUT_RENAMES (SCOP) and get tuples of the
   form (OLD_NAME, NAME1).  If there is a correspondent tuple of
   OLD_NAME in LIVEOUT_BEFORE_GUARD, i.e. (OLD_NAME, NAME2) then
   insert in BB
   
   | RES = phi (NAME1 (on TRUE_EDGE), NAME2 (on FALSE_EDGE))"

   if there is no tuple for OLD_NAME in LIVEOUT_BEFORE_GUARD, insert

   | RES = phi (NAME1 (on TRUE_EDGE),
   |            DEFAULT_DEFINITION of NAME1 (on FALSE_EDGE))".

   Finally register in SCOP_LIVEOUT_RENAMES (scop) the tuple
   (OLD_NAME, RES).  */

static void
insert_guard_phis (scop_p scop, basic_block bb, edge true_edge,
		   edge false_edge, htab_t liveout_before_guard)
{
  struct igp i;
  i.bb = bb;
  i.true_edge = true_edge;
  i.false_edge = false_edge;
  i.liveout_before_guard = liveout_before_guard;

  update_ssa (TODO_update_ssa);
  htab_traverse (SCOP_LIVEOUT_RENAMES (scop), add_guard_exit_phis, &i);
  update_ssa (TODO_update_ssa);
}

/* Helper function for htab_traverse.  */

static int
copy_renames (void **slot, void *s)
{
  struct rename_map_elt_d *entry = (struct rename_map_elt_d *) *slot;
  htab_t res = (htab_t) s;
  tree old_name = entry->old_name;
  tree new_name = entry->new_name;
  struct rename_map_elt_d tmp;
  PTR *x;

  tmp.old_name = old_name;
  x = htab_find_slot (res, &tmp, INSERT);

  if (!*x)
    *x = new_rename_map_elt (old_name, new_name);

  return 1;
}

/* Translates a CLAST statement STMT to GCC representation in the
   context of a SCOP.

   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
     (might be NULL).  
   - IVSTACK contains the surrounding loops around the statement to be
     translated.
*/

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
      htab_t map;
      CloogStatement *cs = ((struct clast_user_stmt *) stmt)->statement;
      graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);

      if (GBB_BB (gbb) == ENTRY_BLOCK_PTR)
	return next_e;

      map = htab_create (10, rename_map_elt_info, eq_rename_map_elts, free);
      loop_iv_stack_patch_for_consts (ivstack, (struct clast_user_stmt *) stmt);
      build_iv_mapping (ivstack, map, gbb, scop);
      next_e = copy_bb_and_scalar_dependences (GBB_BB (gbb), scop,
					       next_e, map);
      htab_delete (map);
      loop_iv_stack_remove_constants (ivstack);
      recompute_all_dominators ();
      update_ssa (TODO_update_ssa);
      graphite_verify ();
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
      last_e = single_succ_edge (split_edge (last_e));
      insert_loop_close_phis (scop, last_e->src);

      recompute_all_dominators ();
      graphite_verify ();
      return translate_clast (scop, context_loop, stmt->next, last_e, ivstack);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    {
      htab_t liveout_before_guard = htab_create (10, rename_map_elt_info,
						 eq_rename_map_elts, free);
      edge last_e = graphite_create_new_guard (scop, next_e,
					       ((struct clast_guard *) stmt),
					       ivstack);
      edge true_e = get_true_edge_from_guard_bb (next_e->dest);
      edge false_e = get_false_edge_from_guard_bb (next_e->dest);
      edge exit_true_e = single_succ_edge (true_e->dest);
      edge exit_false_e = single_succ_edge (false_e->dest);

      htab_traverse (SCOP_LIVEOUT_RENAMES (scop), copy_renames,
		     liveout_before_guard);

      next_e = translate_clast (scop, context_loop, 
				((struct clast_guard *) stmt)->then,
				true_e, ivstack);
      insert_guard_phis (scop, last_e->src, exit_true_e, exit_false_e,
			 liveout_before_guard);
      htab_delete (liveout_before_guard);
      recompute_all_dominators ();
      graphite_verify ();

      return translate_clast (scop, context_loop, stmt->next, last_e, ivstack);
    }

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    {
      next_e = translate_clast (scop, context_loop,
				((struct clast_block *) stmt)->body,
				next_e, ivstack);
      recompute_all_dominators ();
      graphite_verify ();
      return translate_clast (scop, context_loop, stmt->next, next_e, ivstack);
    }

  gcc_unreachable ();
}

/* Free the SCATTERING domain list.  */

static void
free_scattering (CloogDomainList *scattering)
{
  while (scattering)
    {
      CloogDomain *dom = cloog_domain (scattering);
      CloogDomainList *next = cloog_next_domain (scattering);

      cloog_domain_free (dom);
      free (scattering);
      scattering = next;
    }
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
  free_scattering (scattering);

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

  SCOP_PROG (scop) = cloog_program_generate (SCOP_PROG (scop), options);
  stmt = cloog_clast_create (SCOP_PROG (scop), options);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Cloog Output[\n");
      pprint (dump_file, stmt, 0, options);
      cloog_program_dump_cloog (dump_file, SCOP_PROG (scop));
      fprintf (dump_file, "]\n");
    }

  cloog_options_free (options);
  return stmt;
}

/* Remove from the CFG the REGION.  */

static inline void
remove_sese_region (sese region)
{
  VEC (basic_block, heap) *bbs = NULL;
  basic_block entry_bb = SESE_ENTRY (region)->dest;
  basic_block exit_bb = SESE_EXIT (region)->dest;
  basic_block bb;
  int i;

  VEC_safe_push (basic_block, heap, bbs, entry_bb);
  gather_blocks_in_sese_region (entry_bb, exit_bb, &bbs);

  for (i = 0; VEC_iterate (basic_block, bbs, i, bb); i++)
    delete_basic_block (bb);

  VEC_free (basic_block, heap, bbs);
}

typedef struct ifsese_d
{
  sese region;
  sese true_region;
  sese false_region;
} *ifsese;

static inline edge
if_region_entry (ifsese if_region)
{
  return SESE_ENTRY (if_region->region);
}

static inline edge
if_region_exit (ifsese if_region)
{
  return SESE_EXIT (if_region->region);
}

static inline basic_block
if_region_get_condition_block (ifsese if_region)
{
  return if_region_entry (if_region)->dest;
}

static inline void
if_region_set_false_region (ifsese if_region, sese region)
{
  basic_block condition = if_region_get_condition_block (if_region);
  edge false_edge = get_false_edge_from_guard_bb (condition);
  basic_block dummy = false_edge->dest;
  edge entry_region = SESE_ENTRY (region);
  edge exit_region = SESE_EXIT (region);
  basic_block before_region = entry_region->src;
  basic_block last_in_region = exit_region->src;
  void **slot = htab_find_slot_with_hash (current_loops->exits, exit_region,
					  htab_hash_pointer (exit_region),
					  NO_INSERT);

  entry_region->flags = false_edge->flags;
  false_edge->flags = exit_region->flags;

  redirect_edge_pred (entry_region, condition);
  redirect_edge_pred (exit_region, before_region);
  redirect_edge_pred (false_edge, last_in_region);
  redirect_edge_succ (false_edge, single_succ (dummy));
  delete_basic_block (dummy);

  exit_region->flags = EDGE_FALLTHRU;
  recompute_all_dominators ();

  SESE_EXIT (region) = false_edge;
  if_region->false_region = region;

  if (slot)
    {
      struct loop_exit *loop_exit = GGC_CNEW (struct loop_exit);

      memcpy (loop_exit, *((struct loop_exit **) slot), sizeof (struct loop_exit));
      htab_clear_slot (current_loops->exits, slot);

      slot = htab_find_slot_with_hash (current_loops->exits, false_edge,
				       htab_hash_pointer (false_edge),
				       INSERT);
      loop_exit->e = false_edge;
      *slot = loop_exit;
      false_edge->src->loop_father->exits->next = loop_exit;
    }
}

static ifsese
create_if_region_on_edge (edge entry, tree condition)
{
  edge e;
  edge_iterator ei;
  sese sese_region = GGC_NEW (struct sese_d);
  sese true_region = GGC_NEW (struct sese_d);
  sese false_region = GGC_NEW (struct sese_d);
  ifsese if_region = GGC_NEW (struct ifsese_d);
  edge exit = create_empty_if_region_on_edge (entry, condition);

  if_region->region = sese_region;
  if_region->region->entry = entry;
  if_region->region->exit = exit;

  FOR_EACH_EDGE (e, ei, entry->dest->succs)
    {
      if (e->flags & EDGE_TRUE_VALUE)
	{
	  true_region->entry = e;
	  true_region->exit = single_succ_edge (e->dest);
	  if_region->true_region = true_region;
	}
      else if (e->flags & EDGE_FALSE_VALUE)
	{
	  false_region->entry = e;
	  false_region->exit = single_succ_edge (e->dest);
	  if_region->false_region = false_region;
	}
    }

  return if_region;
}

/* Moves REGION in a condition expression:
   | if (1)
   |   ;
   | else
   |   REGION;
*/

static ifsese
move_sese_in_condition (sese region)
{
  basic_block pred_block = split_edge (SESE_ENTRY (region));
  ifsese if_region = NULL;

  SESE_ENTRY (region) = single_succ_edge (pred_block);
  if_region = create_if_region_on_edge (single_pred_edge (pred_block), integer_one_node);
  if_region_set_false_region (if_region, region);

  return if_region;
}

/* Add exit phis for USE on EXIT.  */

static void
scop_add_exit_phis_edge (basic_block exit, tree use, edge false_e, edge true_e)
{
  gimple phi = create_phi_node (use, exit);

  create_new_def_for (gimple_phi_result (phi), phi,
		      gimple_phi_result_ptr (phi));
  add_phi_arg (phi, use, false_e);
  add_phi_arg (phi, use, true_e);
}

/* Add phi nodes for VAR that is used in LIVEIN.  Phi nodes are
   inserted in block BB.  */

static void
scop_add_exit_phis_var (basic_block bb, tree var, bitmap livein,
			edge false_e, edge true_e)
{
  bitmap def;
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (var));

  if (is_gimple_reg (var))
    bitmap_clear_bit (livein, def_bb->index);
  else
    bitmap_set_bit (livein, def_bb->index);

  def = BITMAP_ALLOC (NULL);
  bitmap_set_bit (def, def_bb->index);
  compute_global_livein (livein, def);
  BITMAP_FREE (def);

  scop_add_exit_phis_edge (bb, var, false_e, true_e);
}

/* Insert in the block BB phi nodes for variables defined in REGION
   and used outside the REGION.  The code generation moves REGION in
   the else clause of an "if (1)" and generates code in the then
   clause that is at this point empty:

   | if (1)
   |   empty;
   | else
   |   REGION;
*/

static void
scop_insert_phis_for_liveouts (sese region, basic_block bb,
			       edge false_e, edge true_e)
{
  unsigned i;
  bitmap_iterator bi;

  update_ssa (TODO_update_ssa);

  EXECUTE_IF_SET_IN_BITMAP (SESE_LIVEOUT (region), 0, i, bi)
    scop_add_exit_phis_var (bb, ssa_name (i), SESE_LIVEIN_VER (region, i),
			    false_e, true_e);

  update_ssa (TODO_update_ssa);
}

/* Get the definition of NAME before the SCOP.  Keep track of the
   basic blocks that have been VISITED in a bitmap.  */

static tree
get_vdef_before_scop (scop_p scop, tree name, sbitmap visited)
{
  unsigned i;
  gimple def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (def_stmt);

  if (!def_bb
      || !bb_in_sese_p (def_bb, SCOP_REGION (scop)))
    return name;

  if (TEST_BIT (visited, def_bb->index))
    return NULL_TREE;

  SET_BIT (visited, def_bb->index);

  switch (gimple_code (def_stmt))
    {
    case GIMPLE_PHI:
      for (i = 0; i < gimple_phi_num_args (def_stmt); i++)
	{
	  tree arg = gimple_phi_arg_def (def_stmt, i);
	  tree res = get_vdef_before_scop (scop, arg, visited);
	  if (res)
	    return res;
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

/* Adjust a virtual phi node PHI that is placed at the end of the
   generated code for SCOP:

   | if (1)
   |   generated code from REGION;
   | else
   |   REGION;

   The FALSE_E edge comes from the original code, TRUE_E edge comes
   from the code generated for the SCOP.  */

static void
scop_adjust_vphi (scop_p scop, gimple phi, edge true_e)
{
  unsigned i;

  gcc_assert (gimple_phi_num_args (phi) == 2);

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_edge (phi, i) == true_e)
      {
	tree true_arg, false_arg, before_scop_arg;
	sbitmap visited;

	true_arg = gimple_phi_arg_def (phi, i);
	if (!SSA_NAME_IS_DEFAULT_DEF (true_arg))
	  return;

	false_arg = gimple_phi_arg_def (phi, i == 0 ? 1 : 0);
	if (SSA_NAME_IS_DEFAULT_DEF (false_arg))
	  return;

	visited = sbitmap_alloc (last_basic_block);
	sbitmap_zero (visited);
	before_scop_arg = get_vdef_before_scop (scop, false_arg, visited);
	gcc_assert (before_scop_arg != NULL_TREE);
	SET_PHI_ARG_DEF (phi, i, before_scop_arg);
	sbitmap_free (visited);
      }
}

/* Adjusts the phi nodes in the block BB for variables defined in
   SCOP_REGION and used outside the SCOP_REGION.  The code generation
   moves SCOP_REGION in the else clause of an "if (1)" and generates
   code in the then clause:

   | if (1)
   |   generated code from REGION;
   | else
   |   REGION;

   To adjust the phi nodes after the condition, SCOP_LIVEOUT_RENAMES
   hash table is used: this stores for a name that is part of the
   LIVEOUT of SCOP_REGION its new name in the generated code.  */

static void
scop_adjust_phis_for_liveouts (scop_p scop, basic_block bb, edge false_e,
			       edge true_e)
{
  gimple_stmt_iterator si;

  for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
    {
      unsigned i;
      unsigned false_i = 0;
      gimple phi = gsi_stmt (si);

      if (!is_gimple_reg (PHI_RESULT (phi)))
	{
	  scop_adjust_vphi (scop, phi, true_e);
	  continue;
	}

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	if (gimple_phi_arg_edge (phi, i) == false_e)
	  {
	    false_i = i;
	    break;
	  }

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	if (gimple_phi_arg_edge (phi, i) == true_e)
	  {
	    tree old_name = gimple_phi_arg_def (phi, false_i);
	    tree new_name = get_new_name_from_old_name
	      (SCOP_LIVEOUT_RENAMES (scop), old_name);

	    gcc_assert (old_name != new_name);
	    SET_PHI_ARG_DEF (phi, i, new_name);
	  }
    }
}

/* Returns the first cloog name used in EXPR.  */

static const char *
find_cloog_iv_in_expr (struct clast_expr *expr)
{
  struct clast_term *term = (struct clast_term *) expr;

  if (expr->type == expr_term
      && !term->var)
    return NULL;

  if (expr->type == expr_term)
    return term->var;

  if (expr->type == expr_red)
    {
      int i;
      struct clast_reduction *red = (struct clast_reduction *) expr;

      for (i = 0; i < red->n; i++)
	{
	  const char *res = find_cloog_iv_in_expr ((red)->elts[i]);

	  if (res)
	    return res;
	}
    }

  return NULL;
}

/* Build for a clast_user_stmt USER_STMT a map between the CLAST
   induction variables and the corresponding GCC old induction
   variables.  This information is stored on each GRAPHITE_BB.  */

static void
compute_cloog_iv_types_1 (graphite_bb_p gbb,
			  struct clast_user_stmt *user_stmt)
{
  struct clast_stmt *t;
  int index = 0;

  for (t = user_stmt->substitutions; t; t = t->next, index++)
    {
      PTR *slot;
      struct ivtype_map_elt_d tmp;
      struct clast_expr *expr = (struct clast_expr *) 
	((struct clast_assignment *)t)->RHS;

      /* Create an entry (clast_var, type).  */
      tmp.cloog_iv = find_cloog_iv_in_expr (expr);
      if (!tmp.cloog_iv)
	continue;

      slot = htab_find_slot (GBB_CLOOG_IV_TYPES (gbb), &tmp, INSERT);

      if (!*slot)
	{
	  loop_p loop = gbb_loop_at_index (gbb, index);
	  tree oldiv = oldiv_for_loop (GBB_SCOP (gbb), loop);
	  tree type = oldiv ? TREE_TYPE (oldiv) : integer_type_node;
	  *slot = new_ivtype_map_elt (tmp.cloog_iv, type);
	}
    }
}

/* Walk the CLAST tree starting from STMT and build for each
   clast_user_stmt a map between the CLAST induction variables and the
   corresponding GCC old induction variables.  This information is
   stored on each GRAPHITE_BB.  */

static void
compute_cloog_iv_types (struct clast_stmt *stmt)
{
  if (!stmt)
    return;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    goto next;

  if (CLAST_STMT_IS_A (stmt, stmt_user))
    {
      CloogStatement *cs = ((struct clast_user_stmt *) stmt)->statement;
      graphite_bb_p gbb = (graphite_bb_p) cloog_statement_usr (cs);
      GBB_CLOOG_IV_TYPES (gbb) = htab_create (10, ivtype_map_elt_info,
					      eq_ivtype_map_elts, free);
      compute_cloog_iv_types_1 (gbb, (struct clast_user_stmt *) stmt);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    {
      struct clast_stmt *s = ((struct clast_for *) stmt)->body;
      compute_cloog_iv_types (s);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    {
      struct clast_stmt *s = ((struct clast_guard *) stmt)->then;
      compute_cloog_iv_types (s);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    {
      struct clast_stmt *s = ((struct clast_block *) stmt)->body;
      compute_cloog_iv_types (s);
      goto next;
    }

  gcc_unreachable ();

 next:
  compute_cloog_iv_types (stmt->next);
}

/* GIMPLE Loop Generator: generates loops from STMT in GIMPLE form for
   the given SCOP.  Return true if code generation succeeded.  */

static bool
gloog (scop_p scop, struct clast_stmt *stmt)
{
  edge new_scop_exit_edge = NULL;
  VEC (iv_stack_entry_p, heap) *ivstack = VEC_alloc (iv_stack_entry_p, heap,
						     10);
  loop_p context_loop;
  ifsese if_region = NULL;

  recompute_all_dominators ();
  graphite_verify ();
  if_region = move_sese_in_condition (SCOP_REGION (scop));
  sese_build_livein_liveouts (SCOP_REGION (scop));
  scop_insert_phis_for_liveouts (SCOP_REGION (scop),
				 if_region->region->exit->src,
				 if_region->false_region->exit,
				 if_region->true_region->exit);
  recompute_all_dominators ();
  graphite_verify ();
  context_loop = SESE_ENTRY (SCOP_REGION (scop))->src->loop_father;
  compute_cloog_iv_types (stmt);

  new_scop_exit_edge = translate_clast (scop, context_loop, stmt,
					if_region->true_region->entry,
					&ivstack);
  free_loop_iv_stack (&ivstack);
  cloog_clast_free (stmt);

  graphite_verify ();
  scop_adjust_phis_for_liveouts (scop,
				 if_region->region->exit->src,
				 if_region->false_region->exit,
				 if_region->true_region->exit);

  recompute_all_dominators ();
  graphite_verify ();
  return true;
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

/* Copies the OLD_ROW constraint from OLD_DOMAIN to the NEW_DOMAIN at
   row NEW_ROW.  */

static void
copy_constraint (CloogMatrix *old_domain, CloogMatrix *new_domain,
		 int old_row, int new_row)
{
  int i;

  gcc_assert (old_domain->NbColumns == new_domain->NbColumns
	      && old_row < old_domain->NbRows
	      && new_row < new_domain->NbRows);

  for (i = 0; i < old_domain->NbColumns; i++)
    value_assign (new_domain->p[new_row][i], old_domain->p[old_row][i]);
}

/* Swap coefficients of variables X and Y on row R.   */

static void
swap_constraint_variables (CloogMatrix *domain,
			   int r, int x, int y)
{
  value_swap (domain->p[r][x], domain->p[r][y]);
}

/* Scale by X the coefficient C of constraint at row R in DOMAIN.  */

static void
scale_constraint_variable (CloogMatrix *domain,
			   int r, int c, int x)
{
  Value strip_size_value;
  value_init (strip_size_value);
  value_set_si (strip_size_value, x);
  value_multiply (domain->p[r][c], domain->p[r][c], strip_size_value);
  value_clear (strip_size_value);
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

  gcc_assert (loop_depth <= gbb_nb_loops (gb) - 1);

  VEC_safe_insert (loop_p, heap, GBB_LOOPS (gb), loop_depth, NULL);

  GBB_DOMAIN (gb) = new_domain;

  for (row = 0; row < domain->NbRows; row++)
    for (col = 0; col < domain->NbColumns; col++)
      if (col <= loop_depth)
	value_assign (new_domain->p[row][col], domain->p[row][col]);
      else
	value_assign (new_domain->p[row][col + 1], domain->p[row][col]);

  row = domain->NbRows;

  /* Lower bound of the outer stripped loop.  */
  copy_constraint (new_domain, new_domain,
		   get_lower_bound_row (new_domain, col_loop_old), row);
  swap_constraint_variables (new_domain, row, col_loop_old, col_loop_strip);
  row++;

  /* Upper bound of the outer stripped loop.  */
  copy_constraint (new_domain, new_domain,
		   get_upper_bound_row (new_domain, col_loop_old), row);
  swap_constraint_variables (new_domain, row, col_loop_old, col_loop_strip);
  scale_constraint_variable (new_domain, row, col_loop_strip, stride);
  row++;

  /* Lower bound of a tile starts at "stride * outer_iv".  */
  row = get_lower_bound_row (new_domain, col_loop_old);
  value_set_si (new_domain->p[row][0], 1);
  value_set_si (new_domain->p[row][const_column_index (new_domain)], 0);
  value_set_si (new_domain->p[row][col_loop_old], 1);
  value_set_si (new_domain->p[row][col_loop_strip], -1 * stride);

  /* Upper bound of a tile stops at "stride * outer_iv + stride - 1",
     or at the old upper bound that is not modified.  */
  row = new_domain->NbRows - 1;
  value_set_si (new_domain->p[row][0], 1);
  value_set_si (new_domain->p[row][col_loop_old], -1);
  value_set_si (new_domain->p[row][col_loop_strip], stride);
  value_set_si (new_domain->p[row][const_column_index (new_domain)],
		stride - 1);

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
		   loop->num);
	  fprintf (dump_file, "number of iterations is too low.\n");
	}
    }
  
  return res;
}
 
/* Determines when the interchange of LOOP_A and LOOP_B belonging to
   SCOP is legal.  DEPTH is the number of loops around.  */

static bool
is_interchange_valid (scop_p scop, int loop_a, int loop_b, int depth)
{
  bool res;
  VEC (ddr_p, heap) *dependence_relations;
  VEC (data_reference_p, heap) *datarefs;

  struct loop *nest = VEC_index (loop_p, SCOP_LOOP_NEST (scop), loop_a);
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
      if (!is_interchange_valid (scop, i, j, nb_loops))
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

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nLoops containing BB %d will be loop blocked.\n",
	     GBB_BB (gb)->index);

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
  int stride_size = 51;

  for (i = 0; VEC_iterate (graphite_bb_p, bbs, i, gb); i++)
    transform_done |= graphite_trans_bb_block (gb, stride_size, loops);

  return transform_done;
}

/* Loop block all basic blocks of SCOP.  Return false when the
   transform is not performed.  */

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
    return false;

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
      if (perfect && last_nb_loops - j >= 2)
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
  if (last_nb_loops - j >= 2)
    transform_done |= graphite_trans_loop_block (bbs, last_nb_loops - j);
  VEC_free (graphite_bb_p, heap, bbs);

  return transform_done;
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

static bool
graphite_apply_transformations (scop_p scop)
{
  bool transform_done = false;

  /* Sort the list of bbs.  Keep them always sorted.  */
  graphite_sort_gbbs (scop);

  if (flag_loop_block)
    transform_done = graphite_trans_scop_block (scop);

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE
     Keep in mind that CLooG optimizes in control, so the loop structure
     may change, even if we only use -fgraphite-identity.  */ 
  if (flag_graphite_identity)
    transform_done = true;

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

      if (!build_scop_loop_nests (scop))
	continue;

      for (j = 0; VEC_iterate (loop_p, SCOP_LOOP_NEST (scop), j, loop); j++) 
        if (!loop_in_sese_p (loop_outer (loop), SCOP_REGION (scop)))
          {
	    sd_region open_scop;
	    open_scop.entry = loop->header;
	    open_scop.exit = single_exit (loop)->dest;
	    VEC_safe_push (sd_region, heap, tmp_scops, &open_scop);
	  }
    }

  free_scops (current_scops);
  current_scops = VEC_alloc (scop_p, heap, 3);

  create_sese_edges (tmp_scops);
  build_graphite_scops (tmp_scops);
  VEC_free (sd_region, heap, tmp_scops);
}

/* Perform a set of linear transforms on the loops of the current
   function.  */

void
graphite_transform_loops (void)
{
  int i;
  scop_p scop;
  bool transform_done = false;

  if (number_of_loops () <= 1)
    return;

  current_scops = VEC_alloc (scop_p, heap, 3);
  recompute_all_dominators ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Graphite loop transformations \n");

  initialize_original_copy_tables ();
  cloog_initialize ();
  build_scops ();
  limit_scops ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nnumber of SCoPs: %d\n",
	     VEC_length (scop_p, current_scops));

  for (i = 0; VEC_iterate (scop_p, current_scops, i, scop); i++)
    {
      build_scop_bbs (scop);
      if (!build_scop_loop_nests (scop))
	continue;

      build_bb_loops (scop);

      if (!build_scop_conditions (scop))
	continue;

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

      add_conditions_to_constraints (scop);
      build_scop_canonical_schedules (scop);

      build_scop_data_accesses (scop);
      build_scop_dynamic_schedules (scop);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  int nbrefs = nb_data_refs_in_scop (scop);
	  fprintf (dump_file, "\nnumber of data refs: %d\n", nbrefs);
	}

      if (graphite_apply_transformations (scop))
        transform_done = gloog (scop, find_transform (scop));
#ifdef ENABLE_CHECKING
      else
	{
	  struct clast_stmt *stmt = find_transform (scop);
	  cloog_clast_free (stmt);
	}
#endif
    }

  /* Cleanup.  */
  if (transform_done)
    cleanup_tree_cfg ();

  free_scops (current_scops);
  cloog_finalize ();
  free_original_copy_tables ();
}

#else /* If Cloog is not available: #ifndef HAVE_cloog.  */

void
graphite_transform_loops (void)
{
  sorry ("Graphite loop optimizations cannot be used");
}

#endif
