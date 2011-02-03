/* Instruction scheduling pass.   Log dumping infrastructure.
   Copyright (C) 2006, 2007, 2008, 2010 Free Software Foundation, Inc.

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
#include "tm.h"
#include "diagnostic-core.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "params.h"
#include "output.h"
#include "basic-block.h"
#include "cselib.h"
#include "target.h"

#ifdef INSN_SCHEDULING
#include "sel-sched-ir.h"
#include "sel-sched-dump.h"


/* These variables control high-level pretty printing.  */
static int sel_dump_cfg_flags = SEL_DUMP_CFG_FLAGS;
static int sel_debug_cfg_flags = SEL_DUMP_CFG_FLAGS;

/* True when a cfg should be dumped.  */
static bool sel_dump_cfg_p;

/* Variables that are used to build the cfg dump file name.  */
static const char * const sel_debug_cfg_root = "./";
static const char * const sel_debug_cfg_root_postfix_default = "";
static const char *sel_debug_cfg_root_postfix = "";
static int sel_dump_cfg_fileno = -1;
static int sel_debug_cfg_fileno = -1;

/* When this flag is on, we are dumping to the .dot file.
   When it is off, we are dumping to log.
   This is useful to differentiate formatting between log and .dot
   files.  */
bool sched_dump_to_dot_p = false;

/* Controls how insns from a fence list should be dumped.  */
static int dump_flist_insn_flags = (DUMP_INSN_UID | DUMP_INSN_BBN
                                    | DUMP_INSN_SEQNO);


/* The variable used to hold the value of sched_dump when temporarily
   switching dump output to the other source, e.g. the .dot file.  */
static FILE *saved_sched_dump = NULL;

/* Switch sched_dump to TO.  It must not be called twice.  */
static void
switch_dump (FILE *to)
{
  gcc_assert (saved_sched_dump == NULL);

  saved_sched_dump = sched_dump;
  sched_dump = to;
}

/* Restore previously switched dump.  */
static void
restore_dump (void)
{
  sched_dump = saved_sched_dump;
  saved_sched_dump = NULL;
}


/* Functions for dumping instructions, av sets, and exprs.  */

/* Default flags for dumping insns.  */
static int dump_insn_rtx_flags = DUMP_INSN_RTX_PATTERN;

/* Default flags for dumping vinsns.  */
static int dump_vinsn_flags = (DUMP_VINSN_INSN_RTX | DUMP_VINSN_TYPE
			       | DUMP_VINSN_COUNT);

/* Default flags for dumping expressions.  */
static int dump_expr_flags = DUMP_EXPR_ALL;

/* Default flags for dumping insns when debugging.  */
static int debug_insn_rtx_flags = DUMP_INSN_RTX_ALL;

/* Default flags for dumping vinsns when debugging.  */
static int debug_vinsn_flags = DUMP_VINSN_ALL;

/* Default flags for dumping expressions when debugging.  */
static int debug_expr_flags = DUMP_EXPR_ALL;

/* Controls how an insn from stream should be dumped when debugging.  */
static int debug_insn_flags = DUMP_INSN_ALL;

/* Print an rtx X.  */
void
sel_print_rtl (rtx x)
{
  print_rtl_single (sched_dump, x);
}

/* Dump insn INSN honoring FLAGS.  */
void
dump_insn_rtx_1 (rtx insn, int flags)
{
  int all;

  /* flags == -1 also means dumping all.  */
  all = (flags & 1);;
  if (all)
    flags |= DUMP_INSN_RTX_ALL;

  sel_print ("(");

  if (flags & DUMP_INSN_RTX_UID)
    sel_print ("%d;", INSN_UID (insn));

  if (flags & DUMP_INSN_RTX_PATTERN)
    {
      char buf[2048];

      print_insn (buf, insn, 0);
      sel_print ("%s;", buf);
    }

  if (flags & DUMP_INSN_RTX_BBN)
    {
      basic_block bb = BLOCK_FOR_INSN (insn);

      sel_print ("bb:%d;", bb != NULL ? bb->index : -1);
    }

  sel_print (")");
}


/* Dump INSN with default flags.  */
void
dump_insn_rtx (rtx insn)
{
  dump_insn_rtx_1 (insn, dump_insn_rtx_flags);
}


/* Dump INSN to stderr.  */
DEBUG_FUNCTION void
debug_insn_rtx (rtx insn)
{
  switch_dump (stderr);
  dump_insn_rtx_1 (insn, debug_insn_rtx_flags);
  sel_print ("\n");
  restore_dump ();
}

/* Dump vinsn VI honoring flags.  */
void
dump_vinsn_1 (vinsn_t vi, int flags)
{
  int all;

  /* flags == -1 also means dumping all.  */
  all = flags & 1;
  if (all)
    flags |= DUMP_VINSN_ALL;

  sel_print ("(");

  if (flags & DUMP_VINSN_INSN_RTX)
    dump_insn_rtx_1 (VINSN_INSN_RTX (vi), dump_insn_rtx_flags | all);

  if (flags & DUMP_VINSN_TYPE)
    sel_print ("type:%s;", GET_RTX_NAME (VINSN_TYPE (vi)));

  if (flags & DUMP_VINSN_COUNT)
    sel_print ("count:%d;", VINSN_COUNT (vi));

  if (flags & DUMP_VINSN_COST)
    {
      int cost = vi->cost;

      if (cost != -1)
	sel_print ("cost:%d;", cost);
    }

  sel_print (")");
}

/* Dump vinsn VI with default flags.  */
void
dump_vinsn (vinsn_t vi)
{
  dump_vinsn_1 (vi, dump_vinsn_flags);
}

/* Dump vinsn VI to stderr.  */
DEBUG_FUNCTION void
debug_vinsn (vinsn_t vi)
{
  switch_dump (stderr);
  dump_vinsn_1 (vi, debug_vinsn_flags);
  sel_print ("\n");
  restore_dump ();
}

/* Dump EXPR honoring flags.  */
void
dump_expr_1 (expr_t expr, int flags)
{
  int all;

  /* flags == -1 also means dumping all.  */
  all = flags & 1;
  if (all)
    flags |= DUMP_EXPR_ALL;

  sel_print ("[");

  if (flags & DUMP_EXPR_VINSN)
    dump_vinsn_1 (EXPR_VINSN (expr), dump_vinsn_flags | all);

  if (flags & DUMP_EXPR_SPEC)
    {
      int spec = EXPR_SPEC (expr);

      if (spec != 0)
	sel_print ("spec:%d;", spec);
    }

  if (flags & DUMP_EXPR_USEFULNESS)
    {
      int use = EXPR_USEFULNESS (expr);

      if (use != REG_BR_PROB_BASE)
        sel_print ("use:%d;", use);
    }

  if (flags & DUMP_EXPR_PRIORITY)
    sel_print ("prio:%d;", EXPR_PRIORITY (expr));

  if (flags & DUMP_EXPR_SCHED_TIMES)
    {
      int times = EXPR_SCHED_TIMES (expr);

      if (times != 0)
	sel_print ("times:%d;", times);
    }

  if (flags & DUMP_EXPR_SPEC_DONE_DS)
    {
      ds_t spec_done_ds = EXPR_SPEC_DONE_DS (expr);

      if (spec_done_ds != 0)
	sel_print ("ds:%d;", spec_done_ds);
    }

  if (flags & DUMP_EXPR_ORIG_BB)
    {
      int orig_bb = EXPR_ORIG_BB_INDEX (expr);

      if (orig_bb != 0)
	sel_print ("orig_bb:%d;", orig_bb);
    }

  if (EXPR_TARGET_AVAILABLE (expr) < 1)
    sel_print ("target:%d;", EXPR_TARGET_AVAILABLE (expr));
  sel_print ("]");
}

/* Dump expression EXPR with default flags.  */
void
dump_expr (expr_t expr)
{
  dump_expr_1 (expr, dump_expr_flags);
}

/* Dump expression EXPR to stderr.  */
DEBUG_FUNCTION void
debug_expr (expr_t expr)
{
  switch_dump (stderr);
  dump_expr_1 (expr, debug_expr_flags);
  sel_print ("\n");
  restore_dump ();
}

/* Dump insn I honoring FLAGS.  */
void
dump_insn_1 (insn_t i, int flags)
{
  int all;

  all = flags & 1;
  if (all)
    flags |= DUMP_INSN_ALL;

  if (!sched_dump_to_dot_p)
    sel_print ("(");

  if (flags & DUMP_INSN_EXPR)
    {
      dump_expr_1 (INSN_EXPR (i), dump_expr_flags | all);
      sel_print (";");
    }
  else if (flags & DUMP_INSN_PATTERN)
    {
      dump_insn_rtx_1 (i, DUMP_INSN_RTX_PATTERN | all);
      sel_print (";");
    }
  else if (flags & DUMP_INSN_UID)
    sel_print ("uid:%d;", INSN_UID (i));

  if (flags & DUMP_INSN_SEQNO)
    sel_print ("seqno:%d;", INSN_SEQNO (i));

  if (flags & DUMP_INSN_SCHED_CYCLE)
    {
      int cycle = INSN_SCHED_CYCLE (i);

      if (cycle != 0)
	sel_print ("cycle:%d;", cycle);
    }

  if (!sched_dump_to_dot_p)
    sel_print (")");
}

/* Dump insn I with default flags.  */
void
dump_insn (insn_t i)
{
  dump_insn_1 (i, DUMP_INSN_EXPR | DUMP_INSN_SCHED_CYCLE);
}

/* Dump INSN to stderr.  */
DEBUG_FUNCTION void
debug_insn (insn_t insn)
{
  switch_dump (stderr);
  dump_insn_1 (insn, debug_insn_flags);
  sel_print ("\n");
  restore_dump ();
}

/* Dumps av_set AV.  */
void
dump_av_set (av_set_t av)
{
  av_set_iterator i;
  expr_t expr;

  if (!sched_dump_to_dot_p)
    sel_print ("{");

  FOR_EACH_EXPR (expr, i, av)
    {
      dump_expr (expr);
      if (!sched_dump_to_dot_p)
        sel_print (" ");
      else
        sel_print ("\n");
    }

  if (!sched_dump_to_dot_p)
    sel_print ("}");
}

/* Dumps lvset LV.  */
void
dump_lv_set (regset lv)
{
  sel_print ("{");

  /* This code was adapted from cfg.c: dump_regset ().  */
  if (lv == NULL)
    sel_print ("nil");
  else
    {
      unsigned i;
      reg_set_iterator rsi;
      int count = 0;

      EXECUTE_IF_SET_IN_REG_SET (lv, 0, i, rsi)
        {
          sel_print (" %d", i);
          if (i < FIRST_PSEUDO_REGISTER)
            {
              sel_print (" [%s]", reg_names[i]);
              ++count;
            }

          ++count;

          if (sched_dump_to_dot_p && count == 12)
            {
              count = 0;
              sel_print ("\n");
            }
        }
    }

  sel_print ("}\n");
}

/* Dumps a list of instructions pointed to by P.  */
static void
dump_ilist (ilist_t p)
{
  while (p)
    {
      dump_insn (ILIST_INSN (p));
      p = ILIST_NEXT (p);
    }
}

/* Dumps a list of boundaries pointed to by BNDS.  */
void
dump_blist (blist_t bnds)
{
  for (; bnds; bnds = BLIST_NEXT (bnds))
    {
      bnd_t bnd = BLIST_BND (bnds);

      sel_print ("[to: %d; ptr: ", INSN_UID (BND_TO (bnd)));
      dump_ilist (BND_PTR (bnd));
      sel_print ("] ");
    }
}

/* Dumps a list of fences pointed to by L.  */
void
dump_flist (flist_t l)
{
  while (l)
    {
      dump_insn_1 (FENCE_INSN (FLIST_FENCE (l)), dump_flist_insn_flags);
      sel_print (" ");
      l = FLIST_NEXT (l);
    }
}

/* Dumps an insn vector SUCCS.  */
void
dump_insn_vector (rtx_vec_t succs)
{
  int i;
  rtx succ;

  FOR_EACH_VEC_ELT (rtx, succs, i, succ)
    if (succ)
      dump_insn (succ);
    else
      sel_print ("NULL ");
}

/* Dumps a hard reg set SET to FILE using PREFIX.  */
static void
print_hard_reg_set (FILE *file, const char *prefix, HARD_REG_SET set)
{
  int i;

  fprintf (file, "%s{ ", prefix);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (TEST_HARD_REG_BIT (set, i))
	fprintf (file, "%d ", i);
    }
  fprintf (file, "}\n");
}

/* Dumps a hard reg set SET using PREFIX.  */
void
dump_hard_reg_set (const char *prefix, HARD_REG_SET set)
{
  print_hard_reg_set (sched_dump, prefix, set);
}

/* Pretty print INSN.  This is used as a hook.  */
const char *
sel_print_insn (const_rtx insn, int aligned ATTRIBUTE_UNUSED)
{
  static char buf[80];

  /* '+' before insn means it is a new cycle start and it's not been
     scheduled yet.  '>' - has been scheduled.  */
  if (s_i_d && INSN_LUID (insn) > 0)
    if (GET_MODE (insn) == TImode)
      sprintf (buf, "%s %4d",
               INSN_SCHED_TIMES (insn) > 0 ? "> " : "< ",
               INSN_UID (insn));
    else
      sprintf (buf, "%s %4d",
               INSN_SCHED_TIMES (insn) > 0 ? "! " : "  ",
               INSN_UID (insn));
  else
    if (GET_MODE (insn) == TImode)
      sprintf (buf, "+ %4d", INSN_UID (insn));
    else
      sprintf (buf, "  %4d", INSN_UID (insn));

  return buf;
}


/* Functions for pretty printing of CFG.  */

/* Replace all occurencies of STR1 to STR2 in BUF.
   The BUF must be large enough to hold the result.  */
static void
replace_str_in_buf (char *buf, const char *str1, const char *str2)
{
  int buf_len = strlen (buf);
  int str1_len = strlen (str1);
  int str2_len = strlen (str2);
  int diff = str2_len - str1_len;

  char *p = buf;
  do
    {
      p = strstr (p, str1);
      if (p)
	{
	  char *p1 = p + str1_len;
	  /* Copy the rest of buf and '\0'.  */
	  int n = buf + buf_len - p1;
	  int i;

	  /* Shift str by DIFF chars.  */
	  if (diff > 0)
            for (i = n; i >= 0; i--)
              p1[i + diff] = p1[i];
	  else
            for (i = 0; i <= n; i++)
              p1[i + diff] = p1[i];

	  /* Copy str2.  */
	  for (i = 0; i < str2_len; i++)
	    p[i] = str2[i];

	  p += str2_len;
	  buf_len += diff;
	}

    }
  while (p);
}

/* Replace characters in BUF that have special meaning in .dot file.  */
static void
sel_prepare_string_for_dot_label (char *buf)
{
  static char specials_from[7][2] = { "<", ">", "{", "|", "}", "\"",
                                      "\n" };
  static char specials_to[7][3] = { "\\<", "\\>", "\\{", "\\|", "\\}",
                                    "\\\"", "\\l" };
  unsigned i;

  for (i = 0; i < 7; i++)
    replace_str_in_buf (buf, specials_from[i], specials_to[i]);
}

/* This function acts like printf but dumps to the sched_dump file.  */
void
sel_print (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  if (sched_dump_to_dot_p)
    {
      char *message;
      if (vasprintf (&message, fmt, ap) >= 0 && message != NULL)
	{
	  message = (char *) xrealloc (message, 2 * strlen (message) + 1);
	  sel_prepare_string_for_dot_label (message);
	  fprintf (sched_dump, "%s", message);
	  free (message);
	}
    }
  else
    vfprintf (sched_dump, fmt, ap);
  va_end (ap);
}

/* Dump INSN with FLAGS.  */
static void
sel_dump_cfg_insn (insn_t insn, int flags)
{
  int insn_flags = DUMP_INSN_UID | DUMP_INSN_PATTERN;

  if (sched_luids != NULL && INSN_LUID (insn) > 0)
    {
      if (flags & SEL_DUMP_CFG_INSN_SEQNO)
	insn_flags |= DUMP_INSN_SEQNO | DUMP_INSN_SCHED_CYCLE | DUMP_INSN_EXPR;
    }

  dump_insn_1 (insn, insn_flags);
}

/* Dump E to the dot file F.  */
static void
sel_dump_cfg_edge (FILE *f, edge e)
{
  int w;
  const char *color;

  if (e->flags & EDGE_FALLTHRU)
    {
      w = 10;
      color = ", color = red";
    }
  else if (e->src->next_bb == e->dest)
    {
      w = 3;
      color = ", color = blue";
    }
  else
    {
      w = 1;
      color = "";
    }

  fprintf (f, "\tbb%d -> bb%d [weight = %d%s];\n",
	   e->src->index, e->dest->index, w, color);
}


/* Return true if BB has a predesessor from current region.
   TODO: Either make this function to trace back through empty block
   or just remove those empty blocks.  */
static bool
has_preds_in_current_region_p (basic_block bb)
{
  edge e;
  edge_iterator ei;

  gcc_assert (!in_current_region_p (bb));

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (in_current_region_p (e->src))
      return true;

  return false;
}

/* Dump a cfg region to the dot file F honoring FLAGS.  */
static void
sel_dump_cfg_2 (FILE *f, int flags)
{
  basic_block bb;

  sched_dump_to_dot_p = true;
  switch_dump (f);

  fprintf (f, "digraph G {\n"
	   "\tratio = 2.25;\n"
	   "\tnode [shape = record, fontsize = 9];\n");

  if (flags & SEL_DUMP_CFG_FUNCTION_NAME)
    fprintf (f, "function [label = \"%s\"];\n", current_function_name ());

  FOR_EACH_BB (bb)
    {
      insn_t insn = BB_HEAD (bb);
      insn_t next_tail = NEXT_INSN (BB_END (bb));
      edge e;
      edge_iterator ei;
      bool in_region_p = ((flags & SEL_DUMP_CFG_CURRENT_REGION)
			  && in_current_region_p (bb));
      bool full_p = (!(flags & SEL_DUMP_CFG_CURRENT_REGION)
		     || in_region_p);
      bool some_p = full_p || has_preds_in_current_region_p (bb);
      const char *color;
      const char *style;

      if (!some_p)
	continue;

      if ((flags & SEL_DUMP_CFG_CURRENT_REGION)
	  && in_current_region_p (bb)
	  && BLOCK_TO_BB (bb->index) == 0)
	color = "color = green, ";
      else
	color = "";

      if ((flags & SEL_DUMP_CFG_FENCES)
	  && in_region_p)
	{
	  style = "";

	  if (!sel_bb_empty_p (bb))
	    {
	      bool first_p = true;
	      insn_t tail = BB_END (bb);
	      insn_t cur_insn;

	      cur_insn = bb_note (bb);

	      do
		{
		  fence_t fence;

		  cur_insn = NEXT_INSN (cur_insn);
		  fence = flist_lookup (fences, cur_insn);

		  if (fence != NULL)
		    {
		      if (!FENCE_SCHEDULED_P (fence))
			{
			  if (first_p)
			    color = "color = red, ";
			  else
			    color = "color = yellow, ";
			}
		      else
			color = "color = blue, ";
		    }

		  first_p = false;
		}
	      while (cur_insn != tail);
	    }
	}
      else if (!full_p)
	style = "style = dashed, ";
      else
	style = "";

      fprintf (f, "\tbb%d [%s%slabel = \"{Basic block %d", bb->index,
	       style, color, bb->index);

      if ((flags & SEL_DUMP_CFG_BB_LOOP)
	  && bb->loop_father != NULL)
	fprintf (f, ", loop %d", bb->loop_father->num);

      if (full_p
	  && (flags & SEL_DUMP_CFG_BB_NOTES_LIST))
	{
	  insn_t notes = BB_NOTE_LIST (bb);

	  if (notes != NULL_RTX)
	    {
	      fprintf (f, "|");

	      /* For simplicity, we dump notes from note_list in reversed order
		 to that what they will appear in the code.  */
	      while (notes != NULL_RTX)
		{
		  sel_dump_cfg_insn (notes, flags);
		  fprintf (f, "\\l");

		  notes = PREV_INSN (notes);
		}
	    }
	}

      if (full_p
	  && (flags & SEL_DUMP_CFG_AV_SET)
	  && in_current_region_p (bb)
	  && !sel_bb_empty_p (bb))
	{
	  fprintf (f, "|");

	  if (BB_AV_SET_VALID_P (bb))
	    dump_av_set (BB_AV_SET (bb));
	  else if (BB_AV_LEVEL (bb) == -1)
	    fprintf (f, "AV_SET needs update");
	}

      if ((flags & SEL_DUMP_CFG_LV_SET)
	  && !sel_bb_empty_p (bb))
 	{
	  fprintf (f, "|");

	  if (BB_LV_SET_VALID_P (bb))
	    dump_lv_set (BB_LV_SET (bb));
	  else
	    fprintf (f, "LV_SET needs update");
	}

      if (full_p
	  && (flags & SEL_DUMP_CFG_BB_INSNS))
	{
	  fprintf (f, "|");
	  while (insn != next_tail)
	    {
	      sel_dump_cfg_insn (insn, flags);
	      fprintf (f, "\\l");

	      insn = NEXT_INSN (insn);
	    }
	}

      fprintf (f, "}\"];\n");

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (full_p || in_current_region_p (e->dest))
	  sel_dump_cfg_edge (f, e);
    }

  fprintf (f, "}");

  restore_dump ();
  sched_dump_to_dot_p = false;
}

/* Dump a cfg region to the file specified by TAG honoring flags.
   The file is created by the function.  */
static void
sel_dump_cfg_1 (const char *tag, int flags)
{
  char *buf;
  int i;
  FILE *f;

  ++sel_dump_cfg_fileno;

  if (!sel_dump_cfg_p)
    return;

  i = 1 + snprintf (NULL, 0, "%s/%s%05d-%s.dot", sel_debug_cfg_root,
		    sel_debug_cfg_root_postfix, sel_dump_cfg_fileno, tag);
  buf = XNEWVEC (char, i);
  snprintf (buf, i, "%s/%s%05d-%s.dot", sel_debug_cfg_root,
	    sel_debug_cfg_root_postfix, sel_dump_cfg_fileno, tag);

  f = fopen (buf, "w");

  if (f == NULL)
    fprintf (stderr, "Can't create file: %s.\n", buf);
  else
    {
      sel_dump_cfg_2 (f, flags);

      fclose (f);
    }

  free (buf);
}

/* Setup cfg dumping flags.  Used for debugging.  */
void
setup_dump_cfg_params (void)
{
  sel_dump_cfg_flags = SEL_DUMP_CFG_FLAGS;
  sel_dump_cfg_p = 0;
  sel_debug_cfg_root_postfix = sel_debug_cfg_root_postfix_default;
}

/* Debug a cfg region with FLAGS.  */
void
sel_debug_cfg_1 (int flags)
{
  bool t1 = sel_dump_cfg_p;
  int t2 = sel_dump_cfg_fileno;

  sel_dump_cfg_p = true;
  sel_dump_cfg_fileno = ++sel_debug_cfg_fileno;

  sel_dump_cfg_1 ("sel-debug-cfg", flags);

  sel_dump_cfg_fileno = t2;
  sel_dump_cfg_p = t1;
}

/* Dumps av_set AV to stderr.  */
DEBUG_FUNCTION void
debug_av_set (av_set_t av)
{
  switch_dump (stderr);
  dump_av_set (av);
  sel_print ("\n");
  restore_dump ();
}

/* Dump LV to stderr.  */
DEBUG_FUNCTION void
debug_lv_set (regset lv)
{
  switch_dump (stderr);
  dump_lv_set (lv);
  sel_print ("\n");
  restore_dump ();
}

/* Dump an instruction list P to stderr.  */
DEBUG_FUNCTION void
debug_ilist (ilist_t p)
{
  switch_dump (stderr);
  dump_ilist (p);
  sel_print ("\n");
  restore_dump ();
}

/* Dump a boundary list BNDS to stderr.  */
DEBUG_FUNCTION void
debug_blist (blist_t bnds)
{
  switch_dump (stderr);
  dump_blist (bnds);
  sel_print ("\n");
  restore_dump ();
}

/* Dump an insn vector SUCCS.  */
DEBUG_FUNCTION void
debug_insn_vector (rtx_vec_t succs)
{
  switch_dump (stderr);
  dump_insn_vector (succs);
  sel_print ("\n");
  restore_dump ();
}

/* Dump a hard reg set SET to stderr.  */
DEBUG_FUNCTION void
debug_hard_reg_set (HARD_REG_SET set)
{
  switch_dump (stderr);
  dump_hard_reg_set ("", set);
  sel_print ("\n");
  restore_dump ();
}

/* Debug a cfg region with default flags.  */
void
sel_debug_cfg (void)
{
  sel_debug_cfg_1 (sel_debug_cfg_flags);
}

/* Print a current cselib value for X's address to stderr.  */
DEBUG_FUNCTION rtx
debug_mem_addr_value (rtx x)
{
  rtx t, addr;
  enum machine_mode address_mode;

  gcc_assert (MEM_P (x));
  address_mode = targetm.addr_space.address_mode (MEM_ADDR_SPACE (x));

  t = shallow_copy_rtx (x);
  if (cselib_lookup (XEXP (t, 0), address_mode, 0, GET_MODE (t)))
    XEXP (t, 0) = cselib_subst_to_values (XEXP (t, 0), GET_MODE (t));

  t = canon_rtx (t);
  addr = get_addr (XEXP (t, 0));
  debug_rtx (t);
  debug_rtx (addr);
  return t;
}
#endif

