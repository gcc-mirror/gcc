/* Output routines for graphical representation.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1998.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include <config.h>
#include "system.h"

#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "toplev.h"

static const char *graph_ext[] =
{
  /* no_graph */ "",
  /* vcg */      ".vcg",
};

/* Output text for new basic block.  */
static void
start_fct (fp)
     FILE *fp;
{

  switch (graph_dump_format)
    {
    case vcg:
      fprintf (fp, "\
graph: { title: \"%s\"\nfolding: 1\nhidden: 2\nnode: { title: \"%s.0\" }\n",
	       current_function_name, current_function_name);
      break;
    case no_graph:
      break;
    }
}

static void
start_bb (fp, bb)
     FILE *fp;
     int bb;
{
  switch (graph_dump_format)
    {
    case vcg:
      fprintf (fp, "\
graph: {\ntitle: \"%s.BB%d\"\nfolding: 1\ncolor: lightblue\n\
label: \"basic block %d",
	       current_function_name, bb, bb);
      break;
    case no_graph:
      break;
    }

#if 0
  /* FIXME Should this be printed?  It makes the graph significantly larger. */

  /* Print the live-at-start register list.  */
  fputc ('\n', fp);
  EXECUTE_IF_SET_IN_REG_SET (basic_block_live_at_start[bb], 0, i,
			     {
			       fprintf (fp, " %d", i);
			       if (i < FIRST_PSEUDO_REGISTER)
				 fprintf (fp, " [%s]",
					  reg_names[i]);
			     });
#endif

  switch (graph_dump_format)
    {
    case vcg:
      fputs ("\"\n\n", fp);
      break;
    case no_graph:
      break;
    }
}

static int
node_data (fp, tmp_rtx)
     FILE *fp;
     rtx tmp_rtx;
{
  int result;

  if (PREV_INSN (tmp_rtx) == 0)
    {
      /* This is the first instruction.  Add an edge from the starting
	 block.  */
      switch (graph_dump_format)
	{
	case vcg:
	  fprintf (fp, "\
edge: { sourcename: \"%s.0\" targetname: \"%s.%d\" }\n",
		   current_function_name,
		   current_function_name, XINT (tmp_rtx, 0));
	  break;
	case no_graph:
	  break;
	}
    }

  switch (graph_dump_format)
    {
    case vcg:
      fprintf (fp, "node: {\n  title: \"%s.%d\"\n  color: %s\n  \
label: \"%s %d\n",
	       current_function_name, XINT (tmp_rtx, 0),
	       GET_CODE (tmp_rtx) == NOTE ? "lightgrey"
	       : GET_CODE (tmp_rtx) == INSN ? "green"
	       : GET_CODE (tmp_rtx) == JUMP_INSN ? "darkgreen"
	       : GET_CODE (tmp_rtx) == CALL_INSN ? "darkgreen"
	       : GET_CODE (tmp_rtx) == CODE_LABEL ?  "\
darkgrey\n  shape: ellipse" : "white",
	       GET_RTX_NAME (GET_CODE (tmp_rtx)), XINT (tmp_rtx, 0));
      break;
    case no_graph:
      break;
    }

  /* Print the RTL.  */
  if (GET_CODE (tmp_rtx) == NOTE)
    {
      static const char *note_names[] =
      {
	NULL,
	"deleted",
	"block_beg",
	"block_end",
	"loop_beg",
	"loop_end",
	"function_end",
	"setjmp",
	"loop_cont",
	"loop_vtop",
	"prologue_end",
	"epilogue_beg",
	"deleted_label",
	"function_beg",
	"eh_region_beg",
	"eh_region_end",
	"repeated_line_number",
	"range_start",
	"range_end",
	"live"
      };

      fprintf (fp, " %s",
	       XINT (tmp_rtx, 4) < 0 ? note_names[-XINT (tmp_rtx, 4)] : "");
    }
  else if (GET_RTX_CLASS (GET_CODE (tmp_rtx)) == 'i')
    result = print_rtl_single (fp, PATTERN (tmp_rtx));
  else
    result = print_rtl_single (fp, tmp_rtx);

  switch (graph_dump_format)
    {
    case vcg:
      fputs ("\"\n}\n", fp);
      break;
    case no_graph:
      break;
    }

  return result;
}

static void
draw_edge (fp, from, to, bb_edge, class)
     FILE *fp;
     int from;
     int to;
     int bb_edge;
     int class;
{
  switch (graph_dump_format)
    {
    case vcg:
      fprintf (fp,
	       "edge: { sourcename: \"%s.%d\" targetname: \"%s.%d\" %s",
	       current_function_name, from,
	       current_function_name, to,
	       bb_edge ? "color: blue " : class ? "color: red " : "");
      if (class)
	fprintf (fp, "class: %d ", class);
      fputs ("}\n", fp);
      break;
    case no_graph:
      break;
    }
}

static void
end_bb (fp, bb)
     FILE *fp;
     int bb ATTRIBUTE_UNUSED;
{
  switch (graph_dump_format)
    {
    case vcg:
      fputs ("}\n", fp);
      break;
    case no_graph:
      break;
    }
}

static void
end_fct (fp)
     FILE *fp;
{
  switch (graph_dump_format)
    {
    case vcg:
      fprintf (fp, "node: { title: \"%s.999999\" label: \"END\" }\n}\n",
	       current_function_name);
      break;
    case no_graph:
      break;
    }
}

/* Like print_rtl, but also print out live information for the start of each
   basic block.  */
void
print_rtl_graph_with_bb (base, suffix, rtx_first)
     const char *base;
     const char *suffix;
     rtx rtx_first;
{
  register rtx tmp_rtx;
  size_t namelen = strlen (base);
  size_t suffixlen = strlen (suffix);
  size_t extlen = strlen (graph_ext[graph_dump_format]) + 1;
  char *buf = (char *) alloca (namelen + suffixlen + extlen);
  FILE *fp;

  /* Regenerate the basic block information.  */
  find_basic_blocks (rtx_first, max_reg_num (), NULL);

  memcpy (buf, base, namelen);
  memcpy (buf + namelen, suffix, suffixlen);
  memcpy (buf + namelen + suffixlen, graph_ext[graph_dump_format], extlen);

  fp = fopen (buf, "a");
  if (fp == NULL)
    return;

  if (rtx_first == 0)
    fprintf (fp, "(nil)\n");
  else
    {
      int i, bb;
      enum bb_state { NOT_IN_BB, IN_ONE_BB, IN_MULTIPLE_BB };
      int max_uid = get_max_uid ();
      int *start = (int *) alloca (max_uid * sizeof (int));
      int *end = (int *) alloca (max_uid * sizeof (int));
      enum bb_state *in_bb_p = (enum bb_state *)
	alloca (max_uid * sizeof (enum bb_state));
      /* Element I is a list of I's predecessors/successors.  */
      int_list_ptr *s_preds;
      int_list_ptr *s_succs;
      /* Element I is the number of predecessors/successors of basic
        block I.  */
      int *num_preds;
      int *num_succs;

      for (i = 0; i < max_uid; ++i)
	{
	  start[i] = end[i] = -1;
	  in_bb_p[i] = NOT_IN_BB;
	}

      for (i = n_basic_blocks - 1; i >= 0; --i)
	{
	  rtx x;
	  start[INSN_UID (basic_block_head[i])] = i;
	  end[INSN_UID (basic_block_end[i])] = i;
	  for (x = basic_block_head[i]; x != NULL_RTX; x = NEXT_INSN (x))
	    {
	      in_bb_p[INSN_UID (x)]
		= (in_bb_p[INSN_UID (x)] == NOT_IN_BB)
		 ? IN_ONE_BB : IN_MULTIPLE_BB;
	      if (x == basic_block_end[i])
		break;
	    }
	}

      /* Get the information about the basic blocks predecessors and
	 successors.  */
      s_preds = (int_list_ptr *) alloca (n_basic_blocks
					 * sizeof (int_list_ptr));
      s_succs = (int_list_ptr *) alloca (n_basic_blocks
					 * sizeof (int_list_ptr));
      num_preds = (int *) alloca (n_basic_blocks * sizeof (int));
      num_succs = (int *) alloca (n_basic_blocks * sizeof (int));
      compute_preds_succs (s_preds, s_succs, num_preds, num_succs);

      /* Tell print-rtl that we want graph output.  */
      dump_for_graph = 1;

      /* Start new function.  */
      start_fct (fp);

      for (tmp_rtx = NEXT_INSN (rtx_first); NULL != tmp_rtx;
	   tmp_rtx = NEXT_INSN (tmp_rtx))
	{
	  int did_output;
	  int edge_printed = 0;
	  rtx next_insn;

	  if (start[INSN_UID (tmp_rtx)] < 0 && end[INSN_UID (tmp_rtx)] < 0)
	    {
	      if (GET_CODE (tmp_rtx) == BARRIER)
		continue;
	      if (GET_CODE (tmp_rtx) == NOTE
		  && (1 || in_bb_p[INSN_UID (tmp_rtx)] == NOT_IN_BB))
		continue;
	    }

	  if ((bb = start[INSN_UID (tmp_rtx)]) >= 0)
	    {
	      /* We start a subgraph for each basic block.  */
	      start_bb (fp, bb);

	      if (bb == 0)
		draw_edge (fp, 0, INSN_UID (tmp_rtx), 1, 0);
	    }

	  /* Print the data for this node.  */
	  did_output = node_data (fp, tmp_rtx);
	  next_insn = next_nonnote_insn (tmp_rtx);

	  if ((bb = end[INSN_UID (tmp_rtx)]) >= 0)
	    {
	      int_list_ptr p;

	      /* End of the basic block.  */
	      end_bb (fp, bb);

	      /* Now specify the edges to all the successors of this
		 basic block.  */
	      for (p = s_succs[bb]; p != NULL; p = p->next)
		{
		  int bb_succ = INT_LIST_VAL (p);

		  if (bb_succ >= 0)
		    {
		      rtx block_head = BLOCK_HEAD (bb_succ);

		      draw_edge (fp, INSN_UID (tmp_rtx),
				 INSN_UID (block_head),
				 next_insn != block_head, 0);

		      if (BLOCK_HEAD (bb_succ) == next_insn)
			edge_printed = 1;
		    }
		  else if (bb_succ == EXIT_BLOCK)
		    {
		      draw_edge (fp, INSN_UID (tmp_rtx), 999999,
				 next_insn != 0, 0);

		      if (next_insn == 0)
			edge_printed = 1;
		    }
		  else
		    abort ();
		}
	    }

	  if (!edge_printed)
	    {
	      /* Don't print edges to barriers.  */
	      if (next_insn == 0
		  || GET_CODE (next_insn) != BARRIER)
		draw_edge (fp, XINT (tmp_rtx, 0),
			   next_insn ? INSN_UID (next_insn) : 999999, 0, 0);
	      else
		{
		  /* We draw the remaining edges in class 2.  We have
		     to skip oevr the barrier since these nodes are
		     not printed at all.  */
		  do
		    next_insn = NEXT_INSN (next_insn);
		  while (next_insn
			 && (GET_CODE (next_insn) == NOTE
			     || GET_CODE (next_insn) == BARRIER));

		  draw_edge (fp, XINT (tmp_rtx, 0),
			     next_insn ? INSN_UID (next_insn) : 999999, 0, 2);
		}
	    }
	}

      dump_for_graph = 0;

      end_fct (fp);
    }

  fclose (fp);
}


/* Similar as clean_dump_file, but this time for graph output files.  */
void
clean_graph_dump_file (base, suffix)
     const char *base;
     const char *suffix;
{
  size_t namelen = strlen (base);
  size_t suffixlen = strlen (suffix);
  size_t extlen = strlen (graph_ext[graph_dump_format]) + 1;
  char *buf = (char *) alloca (namelen + extlen + suffixlen);
  FILE *fp;

  memcpy (buf, base, namelen);
  memcpy (buf + namelen, suffix, suffixlen);
  memcpy (buf + namelen + suffixlen, graph_ext[graph_dump_format], extlen);

  fp = fopen (buf, "w");

  if (fp == NULL)
    pfatal_with_name (buf);

  switch (graph_dump_format)
    {
    case vcg:
      fputs ("graph: {\nport_sharing: no\n", fp);
      break;
    case no_graph:
      abort ();
    }

  fclose (fp);
}


/* Do final work on the graph output file.  */
void
finish_graph_dump_file (base, suffix)
     const char *base;
     const char *suffix;
{
  size_t namelen = strlen (base);
  size_t suffixlen = strlen (suffix);
  size_t extlen = strlen (graph_ext[graph_dump_format]) + 1;
  char *buf = (char *) alloca (namelen + suffixlen + extlen);
  FILE *fp;

  memcpy (buf, base, namelen);
  memcpy (buf + namelen, suffix, suffixlen);
  memcpy (buf + namelen + suffixlen, graph_ext[graph_dump_format], extlen);

  fp = fopen (buf, "a");
  if (fp != NULL)
    {
      switch (graph_dump_format)
	{
	case vcg:
	  fputs ("}\n", fp);
	  break;
	case no_graph:
	  abort ();
	}

      fclose (fp);
    }
}
