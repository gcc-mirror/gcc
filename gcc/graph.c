/* Output routines for graphical representation.
   Copyright (C) 1998-2012
   Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1998.
   Rewritten for DOT output by Steven Bosscher, 2012.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h" /* for fatal_error */
#include "sbitmap.h"
#include "basic-block.h"
#include "rtl.h"
#include "tree.h"
#include "graph.h"

/* DOT files with the .dot extension are recognized as document templates
   by a well-known piece of word processing software out of Redmond, WA.
   Therefore some recommend using the .gv extension instead.  Obstinately
   ignore that recommendatition...  */
static const char *const graph_ext = ".dot";

/* Open a file with MODE for dumping our graph to.
   Return the file pointer.  */
static FILE *
open_graph_file (const char *base, const char *mode)
{
  size_t namelen = strlen (base);
  size_t extlen = strlen (graph_ext) + 1;
  char *buf = XALLOCAVEC (char, namelen + extlen);
  FILE *fp;

  memcpy (buf, base, namelen);
  memcpy (buf + namelen, graph_ext, extlen);

  fp = fopen (buf, mode);
  if (fp == NULL)
    fatal_error ("can%'t open %s: %m", buf);

  return fp;
}

/* Print the output from print_insn or print_pattern with GraphViz-special
   characters escaped as necessary.  */
void
print_escaped_line (FILE *fp, const char *buf)
{
  const char *p = buf;

  while (*p)
    {
      switch (*p)
	{
	case '\n':
	  /* Print newlines as a left-aligned newline.  */
	  fputs ("\\l\\\n", fp);
	  break;

	case '{':
	case '}':
	case '<':
	case '>':
	case '|':
	case '"':
	case ' ':
	  /* These characters have to be escaped to work with record-shape nodes.  */
	  fputc ('\\', fp);
	  /* fall through */
	default:
	  fputc (*p, fp);
	  break;
	}
      p++;
    }
  fputs ("\\l\\\n", fp);
}

/* Draw a basic block BB belonging to the function with FNDECL_UID
   as its unique number.  */
static void
draw_cfg_node (FILE *fp, int fndecl_uid, basic_block bb)
{
  rtx insn;
  bool first = true;
  const char *shape;
  const char *fillcolor;

  if (bb->index == ENTRY_BLOCK || bb->index == EXIT_BLOCK)
    {
      shape = "Mdiamond";
      fillcolor = "white";
    }
  else
    {
      shape = "record";
      fillcolor =
	BB_PARTITION (bb) == BB_HOT_PARTITION ? "lightpink"
	: BB_PARTITION (bb) == BB_COLD_PARTITION ? "lightblue"
	: "lightgrey";
    }

  fprintf (fp,
	   "\tfn_%d_basic_block_%d [shape=%s,style=filled,fillcolor=%s,label=\"",
	   fndecl_uid, bb->index, shape, fillcolor);

  if (bb->index == ENTRY_BLOCK)
    fputs ("ENTRY", fp);
  else if (bb->index == EXIT_BLOCK)
    fputs ("EXIT", fp);
  else
    {
      fputc ('{', fp);
      /* TODO: inter-bb stuff.  */
      FOR_BB_INSNS (bb, insn)
	{
	  char buf[2048];

	  if (! first)
	    fputc ('|', fp);

	  print_insn (buf, insn, 1);
	  print_escaped_line (fp, buf);
	  if (INSN_P (insn) && REG_NOTES (insn))
	    for (rtx note = REG_NOTES (insn); note; note = XEXP (note, 1))
	      {
		fprintf (fp, "      %s: ",
			 GET_REG_NOTE_NAME (REG_NOTE_KIND (note)));
		print_pattern (buf, XEXP (note, 0), 1);
		print_escaped_line (fp, buf);
	      }

	  first = false;
	}
      fputc ('}', fp);
    }

  fputs ("\"];\n\n", fp);
}

/* Draw all successor edges of a basic block BB belonging to the function
   with FNDECL_UID as its unique number.  */
static void
draw_cfg_node_succ_edges (FILE *fp, int fndecl_uid, basic_block bb)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      const char *style = "\"solid,bold\"";
      const char *color = "black";
      int weight = 10;

      if (e->flags & EDGE_FAKE)
	{
	  style = "dotted";
	  color = "green";
	  weight = 0;
	}
      else if (e->flags & EDGE_DFS_BACK)
	{
	  style = "\"dotted,bold\"";
	  color = "blue";
	  weight = 10;
	}
      else if (e->flags & EDGE_FALLTHRU)
	{
	  color = "blue";
	  weight = 100;
	}

      if (e->flags & EDGE_ABNORMAL)
	color = "red";

      fprintf (fp,
	       "\tfn_%d_basic_block_%d:s -> fn_%d_basic_block_%d:n "
	       "[style=%s,color=%s,weight=%d,constraint=%s];\n",
	       fndecl_uid, e->src->index,
	       fndecl_uid, e->dest->index,
	       style, color, weight,
	       (e->flags & (EDGE_FAKE | EDGE_DFS_BACK)) ? "false" : "true");
    }
}

/* Print a graphical representation of the CFG of function FUN.
   Currently only supports RTL in cfgrtl or cfglayout mode, GIMPLE is TODO.  */
void
print_rtl_graph_with_bb (const char *base, tree fndecl)
{
  const char *funcname = fndecl_name (fndecl);
  int fndecl_uid = DECL_UID (fndecl);
  FILE *fp = open_graph_file (base, "a");
  int *rpo = XNEWVEC (int, n_basic_blocks);
  basic_block bb;
  int i, n;

  fprintf (fp,
	   "subgraph \"%s\" {\n"
	   "\tcolor=\"black\";\n"
	   "\tlabel=\"%s\";\n",
	   funcname, funcname);

  /* First print all basic blocks.
     Visit the blocks in reverse post order to get a good ranking
     of the nodes.  */
  n = pre_and_rev_post_order_compute (NULL, rpo, true);
  for (i = 0; i < n; i++)
    draw_cfg_node (fp, fndecl_uid, BASIC_BLOCK (rpo[i]));

  /* Draw all edges at the end to get subgraphs right for GraphViz,
     which requires nodes to be defined before edges to cluster
     nodes properly.

     Draw retreating edges as not constraining, this makes the layout
     of the graph better.  (??? Calling mark_dfs_back may change the
     compiler's behavior when dumping, but computing back edges here
     for ourselves is also not desirable.)  */
  mark_dfs_back_edges ();
  FOR_ALL_BB (bb)
    draw_cfg_node_succ_edges (fp, fndecl_uid, bb);

  fputs ("\t}\n", fp);

  fclose (fp);
}

/* Start the dump of a graph.  */
static void
start_graph_dump (FILE *fp)
{
  fputs ("digraph \"\" {\n"
	 "overlap=false;\n",
	 fp);
}

/* End the dump of a graph.  */
static void
end_graph_dump (FILE *fp)
{
  fputs ("}\n", fp);
}

/* Similar as clean_dump_file, but this time for graph output files.  */
void
clean_graph_dump_file (const char *base)
{
  FILE *fp = open_graph_file (base, "w");
  start_graph_dump (fp);
  fclose (fp);
}


/* Do final work on the graph output file.  */
void
finish_graph_dump_file (const char *base)
{
  FILE *fp = open_graph_file (base, "a");
  end_graph_dump (fp);
  fclose (fp);
}
