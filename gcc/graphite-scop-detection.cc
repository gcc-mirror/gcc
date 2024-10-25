/* Detection of Static Control Parts (SCoP) for Graphite.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Tobias Grosser <grosser@fim.uni-passau.de>.

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

#define INCLUDE_ISL
#define INCLUDE_MEMORY

#include "config.h"

#ifdef HAVE_isl

#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "domwalk.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "graphite.h"

class debug_printer
{
private:
  FILE *dump_file;

public:
  void
  set_dump_file (FILE *f)
  {
    gcc_assert (f);
    dump_file = f;
  }

  friend debug_printer &
  operator<< (debug_printer &output, int i)
  {
    fprintf (output.dump_file, "%d", i);
    return output;
  }

  friend debug_printer &
  operator<< (debug_printer &output, const char *s)
  {
    fprintf (output.dump_file, "%s", s);
    return output;
  }

  friend debug_printer &
  operator<< (debug_printer &output, gimple* stmt)
  {
    print_gimple_stmt (output.dump_file, stmt, 0, TDF_VOPS | TDF_MEMSYMS);
    return output;
  }

  friend debug_printer &
  operator<< (debug_printer &output, tree t)
  {
    print_generic_expr (output.dump_file, t, TDF_SLIM);
    return output;
  }
} dp;

#define DEBUG_PRINT(args) do \
    {								\
      if (dump_file && (dump_flags & TDF_DETAILS)) { args; }	\
    } while (0)

/* Pretty print to FILE all the SCoPs in DOT format and mark them with
   different colors.  If there are not enough colors, paint the
   remaining SCoPs in gray.

   Special nodes:
   - "*" after the node number denotes the entry of a SCoP,
   - "#" after the node number denotes the exit of a SCoP,
   - "()" around the node number denotes the entry or the
     exit nodes of the SCOP.  These are not part of SCoP.  */

DEBUG_FUNCTION void
dot_all_sese (FILE *file, vec<sese_l>& scops)
{
  /* Disable debugging while printing graph.  */
  dump_flags_t tmp_dump_flags = dump_flags;
  dump_flags = TDF_NONE;

  fprintf (file, "digraph all {\n");

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      int part_of_scop = false;

      /* Use HTML for every bb label.  So we are able to print bbs
	 which are part of two different SCoPs, with two different
	 background colors.  */
      fprintf (file, "%d [label=<\n  <TABLE BORDER=\"0\" CELLBORDER=\"1\" ",
	       bb->index);
      fprintf (file, "CELLSPACING=\"0\">\n");

      /* Select color for SCoP.  */
      sese_l *region;
      int i;
      FOR_EACH_VEC_ELT (scops, i, region)
	{
	  bool sese_in_region = bb_in_sese_p (bb, *region);
	  if (sese_in_region || (region->exit->dest == bb)
	      || (region->entry->dest == bb))
	    {
	      const char *color;
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

	      fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"%s\">",
		       color);

	      if (!sese_in_region)
		fprintf (file, " (");

	      if (bb == region->entry->dest && bb == region->exit->dest)
		fprintf (file, " %d*# ", bb->index);
	      else if (bb == region->entry->dest)
		fprintf (file, " %d* ", bb->index);
	      else if (bb == region->exit->dest)
		fprintf (file, " %d# ", bb->index);
	      else
		fprintf (file, " %d ", bb->index);

	      fprintf (file, "{lp_%d}", bb->loop_father->num);

	      if (!sese_in_region)
		fprintf (file, ")");

	      fprintf (file, "</TD></TR>\n");
	      part_of_scop = true;
	    }
	}

	if (!part_of_scop)
	  {
	    fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"#ffffff\">");
	    fprintf (file, " %d {lp_%d} </TD></TR>\n", bb->index,
		     bb->loop_father->num);
	  }
	fprintf (file, "  </TABLE>>, shape=box, style=\"setlinewidth(0)\"]\n");
    }

    FOR_ALL_BB_FN (bb, cfun)
      {
	edge e;
	edge_iterator ei;
	FOR_EACH_EDGE (e, ei, bb->succs)
	  fprintf (file, "%d -> %d;\n", bb->index, e->dest->index);
      }

  fputs ("}\n\n", file);

  /* Enable debugging again.  */
  dump_flags = tmp_dump_flags;
}

/* Display SCoP on stderr.  */

DEBUG_FUNCTION void
dot_sese (sese_l& scop)
{
  vec<sese_l> scops;
  scops.create (1);

  if (scop)
    scops.safe_push (scop);

  dot_all_sese (stderr, scops);

  scops.release ();
}

DEBUG_FUNCTION void
dot_cfg ()
{
  vec<sese_l> scops;
  scops.create (1);
  dot_all_sese (stderr, scops);
  scops.release ();
}

/* Returns a COND_EXPR statement when BB has a single predecessor, the
   edge between BB and its predecessor is not a loop exit edge, and
   the last statement of the single predecessor is a COND_EXPR.  */

static gcond *
single_pred_cond_non_loop_exit (basic_block bb)
{
  if (single_pred_p (bb))
    {
      basic_block pred = single_pred (bb);

      if (loop_depth (pred->loop_father) > loop_depth (bb->loop_father))
	return NULL;

      return safe_dyn_cast <gcond *> (*gsi_last_bb (pred));
    }

  return NULL;
}

namespace
{

/* Build the maximal scop containing LOOPs and add it to SCOPS.  */

class scop_detection
{
public:
  scop_detection () : scops (vNULL) {}

  ~scop_detection ()
  {
    scops.release ();
  }

  /* A marker for invalid sese_l.  */
  static sese_l invalid_sese;

  /* Return the SCOPS in this SCOP_DETECTION.  */

  vec<sese_l>
  get_scops ()
  {
    return scops;
  }

  /* Return an sese_l around the LOOP.  */

  sese_l get_sese (loop_p loop);

  /* Merge scops at same loop depth and returns the new sese.
     Returns a new SESE when merge was successful, INVALID_SESE otherwise.  */

  sese_l merge_sese (sese_l first, sese_l second) const;

  /* Build scop outer->inner if possible.  */

  void build_scop_depth (loop_p loop);

  /* Return true when BEGIN is the preheader edge of a loop with a single exit
     END.  */

  static bool region_has_one_loop (sese_l s);

  /* Add to SCOPS a scop starting at SCOP_BEGIN and ending at SCOP_END.  */

  void add_scop (sese_l s);

  /* Returns true if S1 subsumes/surrounds S2.  */
  static bool subsumes (sese_l s1, sese_l s2);

  /* Remove a SCoP which is subsumed by S1.  */
  void remove_subscops (sese_l s1);

  /* Returns true if S1 intersects with S2.  Since we already know that S1 does
     not subsume S2 or vice-versa, we only check for entry bbs.  */

  static bool intersects (sese_l s1, sese_l s2);

  /* Remove one of the scops when it intersects with any other.  */

  void remove_intersecting_scops (sese_l s1);

  /* Return true when a statement in SCOP cannot be represented by Graphite.  */

  bool harmful_loop_in_region (sese_l scop) const;

  /* Return true only when STMT is simple enough for being handled by Graphite.
     This depends on SCOP, as the parameters are initialized relatively to
     this basic block, the linear functions are initialized based on the
     outermost loop containing STMT inside the SCOP.  BB is the place where we
     try to evaluate the STMT.  */

  bool stmt_simple_for_scop_p (sese_l scop, gimple *stmt,
			       basic_block bb) const;

  /* Something like "n * m" is not allowed.  */

  static bool graphite_can_represent_init (tree e);

  /* Return true when SCEV can be represented in the polyhedral model.

     An expression can be represented, if it can be expressed as an
     affine expression.  For loops (i, j) and parameters (m, n) all
     affine expressions are of the form:

     x1 * i + x2 * j + x3 * m + x4 * n + x5 * 1 where x1..x5 element of Z

     1 i + 20 j + (-2) m + 25

     Something like "i * n" or "n * m" is not allowed.  */

  static bool graphite_can_represent_scev (sese_l scop, tree scev);

  /* Return true when EXPR can be represented in the polyhedral model.

     This means an expression can be represented, if it is linear with respect
     to the loops and the strides are non parametric.  LOOP is the place where
     the expr will be evaluated.  SCOP defines the region we analyse.  */

  static bool graphite_can_represent_expr (sese_l scop, loop_p loop,
					   tree expr);

  /* Return true if the data references of STMT can be represented by Graphite.
     We try to analyze the data references in a loop contained in the SCOP.  */

  static bool stmt_has_simple_data_refs_p (sese_l scop, gimple *stmt);

  /* Remove the close phi node at GSI and replace its rhs with the rhs
     of PHI.  */

  static void remove_duplicate_close_phi (gphi *phi, gphi_iterator *gsi);

  /* Returns true when Graphite can represent LOOP in SCOP.
     FIXME: For the moment, graphite cannot be used on loops that iterate using
     induction variables that wrap.  */

  static bool can_represent_loop (loop_p loop, sese_l scop);

  /* Returns the number of pbbs that are in loops contained in SCOP.  */

  static int nb_pbbs_in_loops (scop_p scop);

private:
  vec<sese_l> scops;
};

sese_l scop_detection::invalid_sese (NULL, NULL);

/* Return an sese_l around the LOOP.  */

sese_l
scop_detection::get_sese (loop_p loop)
{
  if (!loop)
    return invalid_sese;

  edge scop_begin = loop_preheader_edge (loop);
  edge scop_end = single_exit (loop);
  if (!scop_end || (scop_end->flags & (EDGE_COMPLEX|EDGE_FAKE)))
    return invalid_sese;

  return sese_l (scop_begin, scop_end);
}

/* Merge scops at same loop depth and returns the new sese.
   Returns a new SESE when merge was successful, INVALID_SESE otherwise.  */

sese_l
scop_detection::merge_sese (sese_l first, sese_l second) const
{
  /* In the trivial case first/second may be NULL.  */
  if (!first)
    return second;
  if (!second)
    return first;

  DEBUG_PRINT (dp << "[scop-detection] try merging sese s1: ";
	       print_sese (dump_file, first);
	       dp << "[scop-detection] try merging sese s2: ";
	       print_sese (dump_file, second));

  auto_bitmap worklist, in_sese_region;
  bitmap_set_bit (worklist, get_entry_bb (first)->index);
  bitmap_set_bit (worklist, get_exit_bb (first)->index);
  bitmap_set_bit (worklist, get_entry_bb (second)->index);
  bitmap_set_bit (worklist, get_exit_bb (second)->index);
  edge entry = NULL, exit = NULL;

  /* We can optimize the case of adding a loop entry dest or exit
     src to the worklist (for single-exit loops) by skipping
     directly to the exit dest / entry src.  in_sese_region
     doesn't have to cover all blocks in the region but merely
     its border it acts more like a visited bitmap.  */
  do
    {
      int index = bitmap_clear_first_set_bit (worklist);
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, index);
      edge_iterator ei;
      edge e;

      /* With fake exit edges we can end up with no possible exit.  */
      if (index == EXIT_BLOCK)
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] cannot merge seses.\n");
	  return invalid_sese;
	}

      bitmap_set_bit (in_sese_region, bb->index);

      basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src == dom
	    && (! entry
		|| dominated_by_p (CDI_DOMINATORS, entry->dest, bb)))
	  {
	    if (entry
		&& ! bitmap_bit_p (in_sese_region, entry->src->index))
	      bitmap_set_bit (worklist, entry->src->index);
	    entry = e;
	  }
	else if (! bitmap_bit_p (in_sese_region, e->src->index))
	  bitmap_set_bit (worklist, e->src->index);

      basic_block pdom = get_immediate_dominator (CDI_POST_DOMINATORS, bb);
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest == pdom
	    && (! exit
		|| dominated_by_p (CDI_POST_DOMINATORS, exit->src, bb)))
	  {
	    if (exit
		&& ! bitmap_bit_p (in_sese_region, exit->dest->index))
	      bitmap_set_bit (worklist, exit->dest->index);
	    exit = e;
	  }
	else if (! bitmap_bit_p (in_sese_region, e->dest->index))
	  bitmap_set_bit (worklist, e->dest->index);
    }
  while (! bitmap_empty_p (worklist));

  sese_l combined (entry, exit);

  DEBUG_PRINT (dp << "[merged-sese] s1: "; print_sese (dump_file, combined));

  return combined;
}

/* Print the loop numbers of the loops contained in SESE to FILE. */

static void
print_sese_loop_numbers (FILE *file, sese_l sese)
{
  bool first_loop = true;
  for (loop_p nest = sese.entry->dest->loop_father; nest; nest = nest->next)
    {
      if (!loop_in_sese_p (nest, sese))
        break;

      for (auto loop : loops_list (cfun, LI_INCLUDE_ROOT, nest))
        {
          gcc_assert (loop_in_sese_p (loop, sese));

          fprintf (file, "%s%d", first_loop ? "" : ", ", loop->num);
          first_loop = false;
        }
    }
}

/* Build scop outer->inner if possible.  */

void
scop_detection::build_scop_depth (loop_p loop)
{
  sese_l s = invalid_sese;
  loop = loop->inner;
  while (loop)
    {
      sese_l next = get_sese (loop);
      if (! next
	  || harmful_loop_in_region (next))
	{
	  if (next)
	    DEBUG_PRINT (dp << "[scop-detection] Discarding SCoP on loops ";
			 print_sese_loop_numbers (dump_file, next);
			 dp << " because of harmful loops\n");
	  if (s)
	    add_scop (s);
	  build_scop_depth (loop);
	  s = invalid_sese;
	}
      else if (! s)
	s = next;
      else
	{
	  sese_l combined = merge_sese (s, next);
	  if (! combined
	      || harmful_loop_in_region (combined))
	    {
	      add_scop (s);
	      s = next;
	    }
	  else
	    s = combined;
	}
      loop = loop->next;
    }
  if (s)
    add_scop (s);
}

/* Returns true when Graphite can represent LOOP in SCOP.
   FIXME: For the moment, graphite cannot be used on loops that iterate using
   induction variables that wrap.  */

bool
scop_detection::can_represent_loop (loop_p loop, sese_l scop)
{
  tree niter;
  struct tree_niter_desc niter_desc;

  /* We can only handle do {} while () style loops correctly.  */
  edge exit = single_exit (loop);
  if (!exit
      || !single_pred_p (loop->latch)
      || exit->src != single_pred (loop->latch)
      || !empty_block_p (loop->latch))
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] Loop shape unsupported.\n");
      return false;
    }

  bool edge_irreducible = (loop_preheader_edge (loop)->flags
			   & EDGE_IRREDUCIBLE_LOOP);
  if (edge_irreducible)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] "
			 "Loop is not a natural loop.\n");
      return false;
    }

  bool niter_is_unconditional = number_of_iterations_exit (loop,
							   single_exit (loop),
							   &niter_desc, false);

  if (!niter_is_unconditional)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] "
			 "Loop niter not unconditional.\n"
			 "Condition: " << niter_desc.assumptions << "\n");
      return false;
    }

  niter = number_of_latch_executions (loop);
  if (!niter)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] Loop niter unknown.\n");
      return false;
    }
  if (!niter_desc.control.no_overflow)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] Loop niter can overflow.\n");
      return false;
    }

  bool undetermined_coefficients = chrec_contains_undetermined (niter);
  if (undetermined_coefficients)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] "
			 "Loop niter chrec contains undetermined "
			 "coefficients.\n");
      return false;
    }

  bool can_represent_expr = graphite_can_represent_expr (scop, loop, niter);
  if (!can_represent_expr)
    {
      DEBUG_PRINT (dp << "[can_represent_loop-fail] "
		      << "Loop niter expression cannot be represented: "
		      << niter << "\n");
      return false;
    }

  return true;
}

/* Return true when BEGIN is the preheader edge of a loop with a single exit
   END.  */

bool
scop_detection::region_has_one_loop (sese_l s)
{
  edge begin = s.entry;
  edge end = s.exit;
  /* Check for a single perfectly nested loop.  */
  if (begin->dest->loop_father->inner)
    return false;

  /* Otherwise, check whether we have adjacent loops.  */
  return (single_pred_p (end->src)
	  && begin->dest->loop_father == single_pred (end->src)->loop_father);
}

/* Add to SCOPS a scop starting at SCOP_BEGIN and ending at SCOP_END.  */

void
scop_detection::add_scop (sese_l s)
{
  gcc_assert (s);

  /* If the exit edge is fake discard the SCoP for now as we're removing the
     fake edges again after analysis.  */
  if (s.exit->flags & EDGE_FAKE)
    {
      DEBUG_PRINT (dp << "[scop-detection-fail] Discarding infinite loop SCoP: ";
		   print_sese (dump_file, s));
      return;
    }

  /* Include the BB with the loop-closed SSA PHI nodes, we need this
     block in the region for code-generating out-of-SSA copies.
     canonicalize_loop_closed_ssa makes sure that is in proper shape.  */
  if (s.exit->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
      && loop_exit_edge_p (s.exit->src->loop_father, s.exit))
    {
      gcc_assert (single_pred_p (s.exit->dest)
		  && single_succ_p (s.exit->dest)
		  && sese_trivially_empty_bb_p (s.exit->dest));
      s.exit = single_succ_edge (s.exit->dest);
    }

  /* Do not add scops with only one loop.  */
  if (region_has_one_loop (s))
    {
      DEBUG_PRINT (dp << "[scop-detection-fail] Discarding one loop SCoP: ";
		   print_sese (dump_file, s));
      return;
    }

  if (get_exit_bb (s) == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      DEBUG_PRINT (dp << "[scop-detection-fail] "
		      << "Discarding SCoP exiting to return: ";
		   print_sese (dump_file, s));
      return;
    }

  /* Remove all the scops which are subsumed by s.  */
  remove_subscops (s);

  /* Remove intersecting scops. FIXME: It will be a good idea to keep
     the non-intersecting part of the scop already in the list.  */
  remove_intersecting_scops (s);

  scops.safe_push (s);
  DEBUG_PRINT (dp << "[scop-detection] Adding SCoP: "; print_sese (dump_file, s));

  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "Loops in SCoP: ");
      print_sese_loop_numbers (dump_file, s);
      fprintf (dump_file, "\n");
    }
}

/* Return true when a statement in SCOP cannot be represented by Graphite.  */

bool
scop_detection::harmful_loop_in_region (sese_l scop) const
{
  basic_block exit_bb = get_exit_bb (scop);
  basic_block entry_bb = get_entry_bb (scop);

  DEBUG_PRINT (dp << "[checking-harmful-bbs] ";
	       print_sese (dump_file, scop));
  gcc_assert (dominated_by_p (CDI_DOMINATORS, exit_bb, entry_bb));

  auto_vec<basic_block> worklist;
  auto_bitmap loops;

  worklist.safe_push (entry_bb);
  while (! worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      DEBUG_PRINT (dp << "Visiting bb_" << bb->index << "\n");

      /* The basic block should not be part of an irreducible loop.  */
      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] Found bb in irreducible "
			     "loop.\n");

	  return true;
	}

      /* Check for unstructured control flow: CFG not generated by structured
	 if-then-else.  */
      if (bb->succs->length () > 1)
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!dominated_by_p (CDI_POST_DOMINATORS, bb, e->dest)
		&& !dominated_by_p (CDI_DOMINATORS, e->dest, bb))
	      {
		DEBUG_PRINT (dp << "[scop-detection-fail] Found unstructured "
				   "control flow.\n");
		return true;
	      }
	}

      /* Collect all loops in the current region.  */
      loop_p loop = bb->loop_father;
      if (loop_in_sese_p (loop, scop))
	bitmap_set_bit (loops, loop->num);

      /* Check for harmful statements in basic blocks part of the region.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	if (!stmt_simple_for_scop_p (scop, gsi_stmt (gsi), bb))
	  {
	    DEBUG_PRINT (dp << "[scop-detection-fail] "
			       "Found harmful statement.\n");
	    return true;
	  }

      for (basic_block dom = first_dom_son (CDI_DOMINATORS, bb);
	   dom;
	   dom = next_dom_son (CDI_DOMINATORS, dom))
	if (dom != scop.exit->dest)
	  worklist.safe_push (dom);
    }

  /* Go through all loops and check that they are still valid in the combined
     scop.  */
  unsigned j;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (loops, 0, j, bi)
    {
      loop_p loop = (*current_loops->larray)[j];
      gcc_assert (loop->num == (int) j);

      /* Check if the loop nests are to be optimized for speed.  */
      if (! loop->inner
	  && ! optimize_loop_for_speed_p (loop))
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] loop_"
		       << loop->num << " is not on a hot path.\n");
	  return true;
	}

      if (! can_represent_loop (loop, scop))
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] cannot represent loop_"
		       << loop->num << "\n");
	  return true;
	}

      /* Check if all loop nests have at least one data reference.
	 ???  This check is expensive and loops premature at this point.
	 If important to retain we can pre-compute this for all innermost
	 loops and reject those when we build a SESE region for a loop
	 during SESE discovery.  */
      if (! loop->inner
	  && ! loop_nest_has_data_refs (loop))
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] loop_" << loop->num
		       << " does not have any data reference.\n");
	  return true;
	}
      DEBUG_PRINT (dp << "[scop-detection] loop_" << loop->num << " is harmless.\n");
    }

  return false;
}

/* Returns true if S1 subsumes/surrounds S2.  */
bool
scop_detection::subsumes (sese_l s1, sese_l s2)
{
  if (dominated_by_p (CDI_DOMINATORS, get_entry_bb (s2),
		      get_entry_bb (s1))
      && dominated_by_p (CDI_POST_DOMINATORS, s2.exit->dest,
			 s1.exit->dest))
    return true;
  return false;
}

/* Remove a SCoP which is subsumed by S1.  */
void
scop_detection::remove_subscops (sese_l s1)
{
  int j;
  sese_l *s2;
  FOR_EACH_VEC_ELT_REVERSE (scops, j, s2)
    {
      if (subsumes (s1, *s2))
	{
	  DEBUG_PRINT (dp << "Removing sub-SCoP";
		       print_sese (dump_file, *s2));
	  scops.unordered_remove (j);
	}
    }
}

/* Returns true if S1 intersects with S2.  Since we already know that S1 does
   not subsume S2 or vice-versa, we only check for entry bbs.  */

bool
scop_detection::intersects (sese_l s1, sese_l s2)
{
  if (dominated_by_p (CDI_DOMINATORS, get_entry_bb (s2),
		      get_entry_bb (s1))
      && !dominated_by_p (CDI_DOMINATORS, get_entry_bb (s2),
			  get_exit_bb (s1)))
    return true;
  if ((s1.exit == s2.entry) || (s2.exit == s1.entry))
    return true;

  return false;
}

/* Remove one of the scops when it intersects with any other.  */

void
scop_detection::remove_intersecting_scops (sese_l s1)
{
  int j;
  sese_l *s2;
  FOR_EACH_VEC_ELT_REVERSE (scops, j, s2)
    {
      if (intersects (s1, *s2))
	{
	  DEBUG_PRINT (dp << "Removing intersecting SCoP";
		       print_sese (dump_file, *s2);
		       dp << "Intersects with:";
		       print_sese (dump_file, s1));
	  scops.unordered_remove (j);
	}
    }
}

/* Something like "n * m" is not allowed.  */

bool
scop_detection::graphite_can_represent_init (tree e)
{
  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      return graphite_can_represent_init (CHREC_LEFT (e))
	&& graphite_can_represent_init (CHREC_RIGHT (e));

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	return graphite_can_represent_init (TREE_OPERAND (e, 0))
	  && tree_fits_shwi_p (TREE_OPERAND (e, 1));
      else
	return graphite_can_represent_init (TREE_OPERAND (e, 1))
	  && tree_fits_shwi_p (TREE_OPERAND (e, 0));

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return graphite_can_represent_init (TREE_OPERAND (e, 0))
	&& graphite_can_represent_init (TREE_OPERAND (e, 1));

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      return graphite_can_represent_init (TREE_OPERAND (e, 0));

    default:
      break;
    }

  return true;
}

/* Return true when SCEV can be represented in the polyhedral model.

   An expression can be represented, if it can be expressed as an
   affine expression.  For loops (i, j) and parameters (m, n) all
   affine expressions are of the form:

   x1 * i + x2 * j + x3 * m + x4 * n + x5 * 1 where x1..x5 element of Z

   1 i + 20 j + (-2) m + 25

   Something like "i * n" or "n * m" is not allowed.  */

bool
scop_detection::graphite_can_represent_scev (sese_l scop, tree scev)
{
  if (chrec_contains_undetermined (scev))
    return false;

  switch (TREE_CODE (scev))
    {
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      return graphite_can_represent_scev (scop, TREE_OPERAND (scev, 0));

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return graphite_can_represent_scev (scop, TREE_OPERAND (scev, 0))
	&& graphite_can_represent_scev (scop, TREE_OPERAND (scev, 1));

    case MULT_EXPR:
      return !CONVERT_EXPR_P (TREE_OPERAND (scev, 0))
	&& !CONVERT_EXPR_P (TREE_OPERAND (scev, 1))
	&& !(chrec_contains_symbols (TREE_OPERAND (scev, 0))
	     && chrec_contains_symbols (TREE_OPERAND (scev, 1)))
	&& graphite_can_represent_init (scev)
	&& graphite_can_represent_scev (scop, TREE_OPERAND (scev, 0))
	&& graphite_can_represent_scev (scop, TREE_OPERAND (scev, 1));

    case POLYNOMIAL_CHREC:
      /* Check for constant strides.  With a non constant stride of
	 'n' we would have a value of 'iv * n'.  Also check that the
	 initial value can represented: for example 'n * m' cannot be
	 represented.  */
      gcc_assert (loop_in_sese_p (get_loop (cfun,
					    CHREC_VARIABLE (scev)), scop));
      if (!evolution_function_right_is_integer_cst (scev)
	  || !graphite_can_represent_init (scev))
	return false;
      return graphite_can_represent_scev (scop, CHREC_LEFT (scev));

    case ADDR_EXPR:
      /* We cannot encode addresses for ISL.  */
      return false;

    default:
      break;
    }

  /* Only affine functions can be represented.  */
  if (tree_contains_chrecs (scev, NULL) || !scev_is_linear_expression (scev))
    return false;

  return true;
}

/* Return true when EXPR can be represented in the polyhedral model.

   This means an expression can be represented, if it is linear with respect to
   the loops and the strides are non parametric.  LOOP is the place where the
   expr will be evaluated.  SCOP defines the region we analyse.  */

bool
scop_detection::graphite_can_represent_expr (sese_l scop, loop_p loop,
					     tree expr)
{
  tree scev = cached_scalar_evolution_in_region (scop, loop, expr);
  bool can_represent = graphite_can_represent_scev (scop, scev);

  if (!can_represent)
    {
      if (dump_file)
	{
	  fprintf (dump_file,
		   "[graphite_can_represent_expr] Cannot represent scev \"");
	  print_generic_expr (dump_file, scev, TDF_SLIM);
	  fprintf (dump_file, "\" of expression ");
	  print_generic_expr (dump_file, expr, TDF_SLIM);
	  fprintf (dump_file, " in loop %d\n", loop->num);
	}
    }
  return can_represent;
}

/* Return true if the data references of STMT can be represented by Graphite.
   We try to analyze the data references in a loop contained in the SCOP.  */

bool
scop_detection::stmt_has_simple_data_refs_p (sese_l scop, gimple *stmt)
{
  edge nest = scop.entry;
  loop_p loop = loop_containing_stmt (stmt);
  if (!loop_in_sese_p (loop, scop))
    loop = NULL;

  auto_vec<data_reference_p> drs;
  if (! graphite_find_data_references_in_stmt (nest, loop, stmt, &drs))
    {
      DEBUG_PRINT (dp << "[stmt_has_simple_data_refs_p] "
			 "Unanalyzable statement.\n");
      return false;
    }

  int j;
  data_reference_p dr;
  FOR_EACH_VEC_ELT (drs, j, dr)
    {
      for (unsigned i = 0; i < DR_NUM_DIMENSIONS (dr); ++i)
	if (! graphite_can_represent_scev (scop, DR_ACCESS_FN (dr, i)))
	  {
	    DEBUG_PRINT (dp << "[stmt_has_simple_data_refs_p] "
			       "Cannot represent access function SCEV: "
			    << DR_ACCESS_FN (dr, i) << "\n");
	    return false;
	  }
    }

  return true;
}

/* GIMPLE_ASM and GIMPLE_CALL may embed arbitrary side effects.
   Calls have side-effects, except those to const or pure
   functions.  */

static bool
stmt_has_side_effects (gimple *stmt)
{
  if (gimple_has_volatile_ops (stmt)
      || (gimple_code (stmt) == GIMPLE_CALL
	  && !(gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE)))
      || (gimple_code (stmt) == GIMPLE_ASM))
    {
      DEBUG_PRINT (dp << "[scop-detection-fail] "
		      << "Statement has side-effects:\n";
	print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS | TDF_MEMSYMS));
      return true;
    }
  return false;
}

/* Return true only when STMT is simple enough for being handled by Graphite.
   This depends on SCOP, as the parameters are initialized relatively to
   this basic block, the linear functions are initialized based on the outermost
   loop containing STMT inside the SCOP.  BB is the place where we try to
   evaluate the STMT.  */

bool
scop_detection::stmt_simple_for_scop_p (sese_l scop, gimple *stmt,
					basic_block bb) const
{
  gcc_assert (scop);

  if (is_gimple_debug (stmt))
    return true;

  if (stmt_has_side_effects (stmt))
    return false;

  if (!stmt_has_simple_data_refs_p (scop, stmt))
    {
      DEBUG_PRINT (dp << "[scop-detection-fail] "
		      << "Graphite cannot handle data-refs in stmt:\n";
	print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS););
      return false;
    }

  switch (gimple_code (stmt))
    {
    case GIMPLE_LABEL:
      return true;

    case GIMPLE_COND:
      {
	/* We can handle all binary comparisons.  Inequalities are
	   also supported as they can be represented with union of
	   polyhedra.  */
	enum tree_code code = gimple_cond_code (stmt);
	if (!(code == LT_EXPR
	      || code == GT_EXPR
	      || code == LE_EXPR
	      || code == GE_EXPR
	      || code == EQ_EXPR
	      || code == NE_EXPR))
	  {
	    DEBUG_PRINT (dp << "[scop-detection-fail] "
			    << "Graphite cannot handle cond stmt:\n";
			 print_gimple_stmt (dump_file, stmt, 0,
					    TDF_VOPS | TDF_MEMSYMS));
	    return false;
	  }

	loop_p loop = bb->loop_father;
	for (unsigned i = 0; i < 2; ++i)
	  {
	    tree op = gimple_op (stmt, i);
	    if (!graphite_can_represent_expr (scop, loop, op))
	      {
		DEBUG_PRINT (dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt,
					      "[scop-detection-fail] "
					      "Graphite cannot represent cond "
					      "stmt operator expression.\n"));
		DEBUG_PRINT (dp << op << "\n");
		return false;
	      }

	    if (! INTEGRAL_TYPE_P (TREE_TYPE (op)))
	      {
		DEBUG_PRINT (dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt,
					      "[scop-detection-fail] "
					      "Graphite cannot represent cond "
					      "statement operator. "
					      "Type must be integral.\n"));
		return false;
	      }
	  }

	return true;
      }

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      {
	tree op, lhs = gimple_get_lhs (stmt);
	ssa_op_iter i;
	/* If we are not going to instantiate the stmt do not require
	   its operands to be instantiatable at this point.  */
	if (lhs
	    && TREE_CODE (lhs) == SSA_NAME
	    && scev_analyzable_p (lhs, scop))
	  return true;
	/* Verify that if we can analyze operands at their def site we
	   also can represent them when analyzed at their uses.  */
	FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
	  if (scev_analyzable_p (op, scop)
	      && chrec_contains_undetermined
		   (cached_scalar_evolution_in_region (scop,
						       bb->loop_father, op)))
	    {
	      DEBUG_PRINT (dp << "[scop-detection-fail] "
			   << "Graphite cannot code-gen stmt:\n";
			   print_gimple_stmt (dump_file, stmt, 0,
					      TDF_VOPS | TDF_MEMSYMS));
	      return false;
	    }
	return true;
      }

    default:
      /* These nodes cut a new scope.  */
      DEBUG_PRINT (
	  dp << "[scop-detection-fail] "
	     << "Gimple stmt not handled in Graphite:\n";
	  print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS | TDF_MEMSYMS));
      return false;
    }
}

/* Returns the number of pbbs that are in loops contained in SCOP.  */

int
scop_detection::nb_pbbs_in_loops (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int res = 0;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    if (loop_in_sese_p (gbb_loop (PBB_BLACK_BOX (pbb)), scop->scop_info->region))
      res++;

  return res;
}

/* Assigns the parameter NAME an index in REGION.  */

static void
assign_parameter_index_in_region (tree name, sese_info_p region)
{
  gcc_assert (TREE_CODE (name) == SSA_NAME
	      && ! defined_in_sese_p (name, region->region));
  int i;
  tree p;
  FOR_EACH_VEC_ELT (region->params, i, p)
    if (p == name)
      return;

  region->params.safe_push (name);
}

/* In the context of sese S, scan the expression E and translate it to
   a linear expression C.  When parsing a symbolic multiplication, K
   represents the constant multiplier of an expression containing
   parameters.  */

static void
scan_tree_for_params (sese_info_p s, tree e)
{
  if (e == chrec_dont_know)
    return;

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      scan_tree_for_params (s, CHREC_LEFT (e));
      break;

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	scan_tree_for_params (s, TREE_OPERAND (e, 0));
      else
	scan_tree_for_params (s, TREE_OPERAND (e, 1));
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0));
      scan_tree_for_params (s, TREE_OPERAND (e, 1));
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0));
      break;

    case SSA_NAME:
      assign_parameter_index_in_region (e, s);
      break;

    case INTEGER_CST:
    case ADDR_EXPR:
    case REAL_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
      break;

   default:
      gcc_unreachable ();
      break;
    }
}

/* Find parameters with respect to REGION in BB. We are looking in memory
   access functions, conditions and loop bounds.  */

static void
find_params_in_bb (sese_info_p region, gimple_poly_bb_p gbb)
{
  /* Find parameters in the access functions of data references.  */
  int i;
  data_reference_p dr;
  FOR_EACH_VEC_ELT (GBB_DATA_REFS (gbb), i, dr)
    for (unsigned j = 0; j < DR_NUM_DIMENSIONS (dr); j++)
      scan_tree_for_params (region, DR_ACCESS_FN (dr, j));

  /* Find parameters in conditional statements.  */
  gimple *stmt;
  FOR_EACH_VEC_ELT (GBB_CONDITIONS (gbb), i, stmt)
    {
      loop_p loop = gimple_bb (stmt)->loop_father;
      tree lhs = cached_scalar_evolution_in_region (region->region, loop,
						    gimple_cond_lhs (stmt));
      tree rhs = cached_scalar_evolution_in_region (region->region, loop,
						    gimple_cond_rhs (stmt));
      gcc_assert (!chrec_contains_undetermined (lhs)
		  && !chrec_contains_undetermined (rhs));

      scan_tree_for_params (region, lhs);
      scan_tree_for_params (region, rhs);
    }
}

/* Record the parameters used in the SCOP BBs.  A variable is a parameter
   in a scop if it does not vary during the execution of that scop.  */

static void
find_scop_parameters (scop_p scop)
{
  unsigned i;
  sese_info_p region = scop->scop_info;

  /* Parameters used in loop bounds are processed during gather_bbs.  */

  /* Find the parameters used in data accesses.  */
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    find_params_in_bb (region, PBB_BLACK_BOX (pbb));

  int nbp = sese_nb_params (region);
  scop_set_nb_params (scop, nbp);
}

static void
add_write (vec<tree> *writes, tree def)
{
  writes->safe_push (def);
  DEBUG_PRINT (dp << "Adding scalar write: ";
	       print_generic_expr (dump_file, def);
	       dp << "\nFrom stmt: ";
	       print_gimple_stmt (dump_file,
				  SSA_NAME_DEF_STMT (def), 0));
}

static void
add_read (vec<scalar_use> *reads, tree use, gimple *use_stmt)
{
  DEBUG_PRINT (dp << "Adding scalar read: ";
	       print_generic_expr (dump_file, use);
	       dp << "\nFrom stmt: ";
	       print_gimple_stmt (dump_file, use_stmt, 0));
  reads->safe_push (std::make_pair (use_stmt, use));
}


/* Record DEF if it is used in other bbs different than DEF_BB in the SCOP.  */

static void
build_cross_bb_scalars_def (scop_p scop, tree def, basic_block def_bb,
			     vec<tree> *writes)
{
  if (!is_gimple_reg (def))
    return;

  bool scev_analyzable = scev_analyzable_p (def, scop->scop_info->region);

  gimple *use_stmt;
  imm_use_iterator imm_iter;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    /* Do not gather scalar variables that can be analyzed by SCEV as they can
       be generated out of the induction variables.  */
    if ((! scev_analyzable
	 /* But gather SESE liveouts as we otherwise fail to rewrite their
	    exit PHIs.  */
	 || ! bb_in_sese_p (gimple_bb (use_stmt), scop->scop_info->region))
	&& (def_bb != gimple_bb (use_stmt) && !is_gimple_debug (use_stmt)))
      {
	add_write (writes, def);
	break;
      }
}

/* Record USE if it is defined in other bbs different than USE_STMT
   in the SCOP.  */

static void
build_cross_bb_scalars_use (scop_p scop, tree use, gimple *use_stmt,
			    vec<scalar_use> *reads)
{
  if (!is_gimple_reg (use))
    return;

  /* Do not gather scalar variables that can be analyzed by SCEV as they can be
     generated out of the induction variables.  */
  if (scev_analyzable_p (use, scop->scop_info->region))
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (use);
  if (gimple_bb (def_stmt) != gimple_bb (use_stmt))
    add_read (reads, use, use_stmt);
}

/* Generates a polyhedral black box only if the bb contains interesting
   information.  */

static gimple_poly_bb_p
try_generate_gimple_bb (scop_p scop, basic_block bb)
{
  vec<data_reference_p> drs = vNULL;
  vec<tree> writes = vNULL;
  vec<scalar_use> reads = vNULL;

  sese_l region = scop->scop_info->region;
  edge nest = region.entry;
  loop_p loop = bb->loop_father;
  if (!loop_in_sese_p (loop, region))
    loop = NULL;

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;

      graphite_find_data_references_in_stmt (nest, loop, stmt, &drs);

      tree def = gimple_get_lhs (stmt);
      if (def)
	build_cross_bb_scalars_def (scop, def, gimple_bb (stmt), &writes);

      ssa_op_iter iter;
      tree use;
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	build_cross_bb_scalars_use (scop, use, stmt, &reads);
    }

  /* Handle defs and uses in PHIs.  Those need special treatment given
     that we have to present ISL with sth that looks like we've rewritten
     the IL out-of-SSA.  */
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res)
	  || scev_analyzable_p (res, scop->scop_info->region))
	continue;
      /* To simulate out-of-SSA the block containing the PHI node has
         reads of the PHI destination.  And to preserve SSA dependences
	 we also write to it (the out-of-SSA decl and the SSA result
	 are coalesced for dependence purposes which is good enough).  */
      add_read (&reads, res, phi);
      add_write (&writes, res);
    }
  basic_block bb_for_succs = bb;
  if (bb_for_succs == bb_for_succs->loop_father->latch
      && bb_in_sese_p (bb_for_succs, scop->scop_info->region)
      && sese_trivially_empty_bb_p (bb_for_succs))
    bb_for_succs = NULL;
  while (bb_for_succs)
    {
      basic_block latch = NULL;
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb_for_succs->succs)
	{
	  for (gphi_iterator psi = gsi_start_phis (e->dest); !gsi_end_p (psi);
	       gsi_next (&psi))
	    {
	      gphi *phi = psi.phi ();
	      tree res = gimple_phi_result (phi);
	      if (virtual_operand_p (res))
		continue;
	      /* To simulate out-of-SSA the predecessor of edges into PHI nodes
		 has a copy from the PHI argument to the PHI destination.  */
	      if (! scev_analyzable_p (res, scop->scop_info->region))
		add_write (&writes, res);
	      tree use = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      if (TREE_CODE (use) == SSA_NAME
		  && ! SSA_NAME_IS_DEFAULT_DEF (use)
		  && gimple_bb (SSA_NAME_DEF_STMT (use)) != bb_for_succs
		  && ! scev_analyzable_p (use, scop->scop_info->region))
		add_read (&reads, use, phi);
	    }
	  if (e->dest == bb_for_succs->loop_father->latch
	      && bb_in_sese_p (e->dest, scop->scop_info->region)
	      && sese_trivially_empty_bb_p (e->dest))
	    latch = e->dest;
	}
      /* Handle empty latch block PHIs here, otherwise we confuse ISL
	 with extra conditional code where it then peels off the last
	 iteration just because of that.  It would be simplest if we
	 just didn't force simple latches (thus remove the forwarder).  */
      bb_for_succs = latch;
    }

  /* For the region exit block add reads for all live-out vars.  */
  if (bb == scop->scop_info->region.exit->src)
    {
      sese_build_liveouts (scop->scop_info);
      unsigned i;
      bitmap_iterator bi;
      EXECUTE_IF_SET_IN_BITMAP (scop->scop_info->liveout, 0, i, bi)
	{
	  tree use = ssa_name (i);
	  add_read (&reads, use, NULL);
	}
    }

  if (drs.is_empty () && writes.is_empty () && reads.is_empty ())
    return NULL;

  return new_gimple_poly_bb (bb, drs, reads, writes);
}

/* Compute alias-sets for all data references in DRS.  */

static bool
build_alias_set (scop_p scop)
{
  int num_vertices = scop->drs.length ();
  struct graph *g = new_graph (num_vertices);
  dr_info *dr1, *dr2;
  int i, j;
  int *all_vertices;

  struct loop *nest
    = find_common_loop (scop->scop_info->region.entry->dest->loop_father,
			scop->scop_info->region.exit->src->loop_father);

  FOR_EACH_VEC_ELT (scop->drs, i, dr1)
    for (j = i+1; scop->drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1->dr, dr2->dr, nest))
	{
	  /* Dependences in the same alias set need to be handled
	     by just looking at DR_ACCESS_FNs.  */
	  if (DR_NUM_DIMENSIONS (dr1->dr) == 0
	      || DR_NUM_DIMENSIONS (dr1->dr) != DR_NUM_DIMENSIONS (dr2->dr)
	      || ! operand_equal_p (DR_BASE_OBJECT (dr1->dr),
				    DR_BASE_OBJECT (dr2->dr),
				    OEP_ADDRESS_OF)
	      || ! types_compatible_p (TREE_TYPE (DR_BASE_OBJECT (dr1->dr)),
				       TREE_TYPE (DR_BASE_OBJECT (dr2->dr))))
	    {
	      free_graph (g);
	      return false;
	    }
	  add_edge (g, i, j);
	  add_edge (g, j, i);
	}

  all_vertices = XNEWVEC (int, num_vertices);
  for (i = 0; i < num_vertices; i++)
    all_vertices[i] = i;

  scop->max_alias_set
    = graphds_dfs (g, all_vertices, num_vertices, NULL, true, NULL) + 1;
  free (all_vertices);

  for (i = 0; i < g->n_vertices; i++)
    scop->drs[i].alias_set = g->vertices[i].component + 1;

  free_graph (g);
  return true;
}

/* Gather BBs and conditions for a SCOP.  */
class gather_bbs : public dom_walker
{
public:
  gather_bbs (cdi_direction, scop_p, int *);

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

private:
  auto_vec<gimple *, 3> conditions, cases;
  scop_p scop;
};
}
gather_bbs::gather_bbs (cdi_direction direction, scop_p scop, int *bb_to_rpo)
  : dom_walker (direction, ALL_BLOCKS, bb_to_rpo), scop (scop)
{
}

/* Call-back for dom_walk executed before visiting the dominated
   blocks.  */

edge
gather_bbs::before_dom_children (basic_block bb)
{
  sese_info_p region = scop->scop_info;
  if (!bb_in_sese_p (bb, region->region))
    return dom_walker::STOP;

  /* For loops fully contained in the region record parameters in the
     loop bounds.  */
  loop_p loop = bb->loop_father;
  if (loop->header == bb
      && loop_in_sese_p (loop, region->region))
    {
      tree nb_iters = number_of_latch_executions (loop);
      if (chrec_contains_symbols (nb_iters))
	{
	  nb_iters = cached_scalar_evolution_in_region (region->region,
							loop, nb_iters);
	  scan_tree_for_params (region, nb_iters);
	}
    }

  if (gcond *stmt = single_pred_cond_non_loop_exit (bb))
    {
      edge e = single_pred_edge (bb);
      /* Make sure the condition is in the region and thus was verified
         to be handled.  */
      if (e != region->region.entry)
	{
	  conditions.safe_push (stmt);
	  if (e->flags & EDGE_TRUE_VALUE)
	    cases.safe_push (stmt);
	  else
	    cases.safe_push (NULL);
	}
    }

  scop->scop_info->bbs.safe_push (bb);

  gimple_poly_bb_p gbb = try_generate_gimple_bb (scop, bb);
  if (!gbb)
    return NULL;

  GBB_CONDITIONS (gbb) = conditions.copy ();
  GBB_CONDITION_CASES (gbb) = cases.copy ();

  poly_bb_p pbb = new_poly_bb (scop, gbb);
  scop->pbbs.safe_push (pbb);

  int i;
  data_reference_p dr;
  FOR_EACH_VEC_ELT (gbb->data_refs, i, dr)
    {
      DEBUG_PRINT (dp << "Adding memory ";
		   if (dr->is_read)
		     dp << "read: ";
		   else
		     dp << "write: ";
		   print_generic_expr (dump_file, dr->ref);
		   dp << "\nFrom stmt: ";
		   print_gimple_stmt (dump_file, dr->stmt, 0));

      scop->drs.safe_push (dr_info (dr, pbb));
    }

  return NULL;
}

/* Call-back for dom_walk executed after visiting the dominated
   blocks.  */

void
gather_bbs::after_dom_children (basic_block bb)
{
  if (!bb_in_sese_p (bb, scop->scop_info->region))
    return;

  if (single_pred_cond_non_loop_exit (bb))
    {
      edge e = single_pred_edge (bb);
      if (e != scop->scop_info->region.entry)
	{
	  conditions.pop ();
	  cases.pop ();
	}
    }
}


/* Compute sth like an execution order, dominator order with first executing
   edges that stay inside the current loop, delaying processing exit edges.  */

static int *bb_to_rpo;

/* Helper for qsort, sorting after order above.  */

static int
cmp_pbbs (const void *pa, const void *pb)
{
  poly_bb_p bb1 = *((const poly_bb_p *)pa);
  poly_bb_p bb2 = *((const poly_bb_p *)pb);
  if (bb_to_rpo[bb1->black_box->bb->index]
      < bb_to_rpo[bb2->black_box->bb->index])
    return -1;
  else if (bb_to_rpo[bb1->black_box->bb->index]
	   > bb_to_rpo[bb2->black_box->bb->index])
    return 1;
  else
    return 0;
}

/* Find Static Control Parts (SCoP) in the current function and pushes
   them to SCOPS.  */

void
build_scops (vec<scop_p> *scops)
{
  if (dump_file)
    dp.set_dump_file (dump_file);

  scop_detection sb;
  sb.build_scop_depth (current_loops->tree_root);

  /* Now create scops from the lightweight SESEs.  */
  vec<sese_l> scops_l = sb.get_scops ();

  /* Domwalk needs a bb to RPO mapping.  Compute it once here.  */
  int *postorder = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  int postorder_num = pre_and_rev_post_order_compute (NULL, postorder, true);
  bb_to_rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  for (int i = 0; i < postorder_num; ++i)
    bb_to_rpo[postorder[i]] = i;
  free (postorder);

  int i;
  sese_l *s;
  FOR_EACH_VEC_ELT (scops_l, i, s)
    {
      scop_p scop = new_scop (s->entry, s->exit);

      /* Record all basic blocks and their conditions in REGION.  */
      gather_bbs (CDI_DOMINATORS, scop, bb_to_rpo).walk (s->entry->dest);

      /* Sort pbbs after execution order for initial schedule generation.  */
      scop->pbbs.qsort (cmp_pbbs);

      if (! build_alias_set (scop))
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] cannot handle dependences\n");
	  free_scop (scop);
	  continue;
	}

      /* Do not optimize a scop containing only PBBs that do not belong
	 to any loops.  */
      if (sb.nb_pbbs_in_loops (scop) == 0)
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] no data references.\n");
	  free_scop (scop);
	  continue;
	}

      unsigned max_arrays = param_graphite_max_arrays_per_scop;
      if (max_arrays > 0
	  && scop->drs.length () >= max_arrays)
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] too many data references: "
		       << scop->drs.length ()
		       << " is larger than --param graphite-max-arrays-per-scop="
		       << max_arrays << ".\n");
	  free_scop (scop);
	  continue;
	}

      find_scop_parameters (scop);
      graphite_dim_t max_dim = param_graphite_max_nb_scop_params;
      if (max_dim > 0
	  && scop_nb_params (scop) > max_dim)
	{
	  DEBUG_PRINT (dp << "[scop-detection-fail] too many parameters: "
			  << scop_nb_params (scop)
			  << " larger than --param graphite-max-nb-scop-params="
			  << max_dim << ".\n");
	  free_scop (scop);
	  continue;
	}

      scops->safe_push (scop);
    }

  free (bb_to_rpo);
  bb_to_rpo = NULL;
  DEBUG_PRINT (dp << "number of SCoPs: " << (scops ? scops->length () : 0););
}

#endif /* HAVE_isl */
