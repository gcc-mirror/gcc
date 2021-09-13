/* Generate code from machine description to emit insns as rtl.
   Copyright (C) 1987-2021 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"


/* Data structure for recording the patterns of insns that have CLOBBERs.
   We use this to output a function that adds these CLOBBERs to a
   previously-allocated PARALLEL expression.  */

struct clobber_pat
{
  struct clobber_ent *insns;
  rtx pattern;
  int first_clobber;
  struct clobber_pat *next;
  int has_hard_reg;
} *clobber_list;

/* Records one insn that uses the clobber list.  */

struct clobber_ent
{
  int code_number;		/* Counts only insns.  */
  struct clobber_ent *next;
};

static void output_peephole2_scratches	(rtx);

/* True for <X>_optab if that optab isn't allowed to fail.  */
static bool nofail_optabs[NUM_OPTABS];

static void
print_code (RTX_CODE code)
{
  const char *p1;
  for (p1 = GET_RTX_NAME (code); *p1; p1++)
    putchar (TOUPPER (*p1));
}

static void
gen_rtx_scratch (rtx x, enum rtx_code subroutine_type)
{
  if (subroutine_type == DEFINE_PEEPHOLE2)
    {
      printf ("operand%d", XINT (x, 0));
    }
  else
    {
      printf ("gen_rtx_SCRATCH (%smode)", GET_MODE_NAME (GET_MODE (x)));
    }
}

/* Print a C expression to construct an RTX just like X,
   substituting any operand references appearing within.  */

static void
gen_exp (rtx x, enum rtx_code subroutine_type, char *used, md_rtx_info *info)
{
  RTX_CODE code;
  int i;
  int len;
  const char *fmt;
  const char *sep = "";

  if (x == 0)
    {
      printf ("NULL_RTX");
      return;
    }

  code = GET_CODE (x);

  switch (code)
    {
    case MATCH_OPERAND:
    case MATCH_DUP:
      if (used)
	{
	  if (used[XINT (x, 0)])
	    {
	      printf ("copy_rtx (operand%d)", XINT (x, 0));
	      return;
	    }
	  used[XINT (x, 0)] = 1;
	}
      printf ("operand%d", XINT (x, 0));
      return;

    case MATCH_OP_DUP:
      printf ("gen_rtx_fmt_");
      for (i = 0; i < XVECLEN (x, 1); i++)
	printf ("e");
      printf (" (GET_CODE (operand%d), ", XINT (x, 0));
      if (GET_MODE (x) == VOIDmode)
	printf ("GET_MODE (operand%d)", XINT (x, 0));
      else
	printf ("%smode", GET_MODE_NAME (GET_MODE (x)));
      for (i = 0; i < XVECLEN (x, 1); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (x, 1, i), subroutine_type, used, info);
	}
      printf (")");
      return;

    case MATCH_OPERATOR:
      printf ("gen_rtx_fmt_");
      for (i = 0; i < XVECLEN (x, 2); i++)
	printf ("e");
      printf (" (GET_CODE (operand%d)", XINT (x, 0));
      printf (", %smode", GET_MODE_NAME (GET_MODE (x)));
      for (i = 0; i < XVECLEN (x, 2); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (x, 2, i), subroutine_type, used, info);
	}
      printf (")");
      return;

    case MATCH_PARALLEL:
    case MATCH_PAR_DUP:
      printf ("operand%d", XINT (x, 0));
      return;

    case MATCH_SCRATCH:
      gen_rtx_scratch (x, subroutine_type);
      return;

    case PC:
      printf ("pc_rtx");
      return;
    case RETURN:
      printf ("ret_rtx");
      return;
    case SIMPLE_RETURN:
      printf ("simple_return_rtx");
      return;
    case CLOBBER:
      if (REG_P (XEXP (x, 0)))
	{
	  printf ("gen_hard_reg_clobber (%smode, %i)",
		  GET_MODE_NAME (GET_MODE (XEXP (x, 0))),
		  REGNO (XEXP (x, 0)));
	  return;
	}
      break;

    case CONST_INT:
      if (INTVAL (x) == 0)
	printf ("const0_rtx");
      else if (INTVAL (x) == 1)
	printf ("const1_rtx");
      else if (INTVAL (x) == -1)
	printf ("constm1_rtx");
      else if (-MAX_SAVED_CONST_INT <= INTVAL (x)
	       && INTVAL (x) <= MAX_SAVED_CONST_INT)
	printf ("const_int_rtx[MAX_SAVED_CONST_INT + (%d)]",
		(int) INTVAL (x));
      else if (INTVAL (x) == STORE_FLAG_VALUE)
	printf ("const_true_rtx");
      else
	{
	  printf ("GEN_INT (");
	  printf (HOST_WIDE_INT_PRINT_DEC_C, INTVAL (x));
	  printf (")");
	}
      return;

    case CONST_DOUBLE:
      /* Handle `const_double_zero' rtx.  */
      if (CONST_DOUBLE_REAL_VALUE (x)->cl == rvc_zero)
	{
	  printf ("CONST_DOUBLE_ATOF (\"0\", %smode)",
		  GET_MODE_NAME (GET_MODE (x)));
	  return;
	}
      /* Fall through.  */
    case CONST_FIXED:
    case CONST_WIDE_INT:
      /* These shouldn't be written in MD files.  Instead, the appropriate
	 routines in varasm.c should be called.  */
      gcc_unreachable ();

    default:
      break;
    }

  printf ("gen_rtx_");
  print_code (code);
  printf (" (");
  if (!always_void_p (code))
    {
      printf ("%smode", GET_MODE_NAME (GET_MODE (x)));
      sep = ",\n\t";
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == '0')
	break;
      fputs (sep, stdout);
      switch (fmt[i])
	{
	case 'e': case 'u':
	  gen_exp (XEXP (x, i), subroutine_type, used, info);
	  break;

	case 'i':
	  printf ("%u", XINT (x, i));
	  break;

	case 'r':
	  printf ("%u", REGNO (x));
	  break;

	case 'p':
	  /* We don't have a way of parsing polynomial offsets yet,
	     and hopefully never will.  */
	  printf ("%d", SUBREG_BYTE (x).to_constant ());
	  break;

	case 's':
	  printf ("\"%s\"", XSTR (x, i));
	  break;

	case 'E':
	  {
	    int j;
	    printf ("gen_rtvec (%d", XVECLEN (x, i));
	    for (j = 0; j < XVECLEN (x, i); j++)
	      {
		printf (",\n\t\t");
		gen_exp (XVECEXP (x, i, j), subroutine_type, used, info);
	      }
	    printf (")");
	    break;
	  }

	default:
	  gcc_unreachable ();
	}
      sep = ",\n\t";
    }
  printf (")");
}

/* Output code to emit the instruction patterns in VEC, with each element
   becoming a separate instruction.  USED is as for gen_exp.  */

static void
gen_emit_seq (rtvec vec, char *used, md_rtx_info *info)
{
  for (int i = 0, len = GET_NUM_ELEM (vec); i < len; ++i)
    {
      bool last_p = (i == len - 1);
      rtx next = RTVEC_ELT (vec, i);
      if (const char *name = get_emit_function (next))
	{
	  printf ("  %s (", name);
	  gen_exp (next, DEFINE_EXPAND, used, info);
	  printf (");\n");
	  if (!last_p && needs_barrier_p (next))
	    printf ("  emit_barrier ();");
	}
      else
	{
	  printf ("  emit (");
	  gen_exp (next, DEFINE_EXPAND, used, info);
	  printf (", %s);\n", last_p ? "false" : "true");
	}
    }
}

/* Emit the given C code to the output file.  The code is allowed to
   fail if CAN_FAIL_P.  NAME describes what we're generating,
   for use in error messages.  */

static void
emit_c_code (const char *code, bool can_fail_p, const char *name)
{
  if (can_fail_p)
    printf ("#define FAIL return (end_sequence (), _val)\n");
  else
    printf ("#define FAIL _Pragma (\"GCC error \\\"%s cannot FAIL\\\"\")"
	    " (void)0\n", name);
  printf ("#define DONE return (_val = get_insns (), "
	  "end_sequence (), _val)\n");

  rtx_reader_ptr->print_md_ptr_loc (code);
  printf ("%s\n", code);

  printf ("#undef DONE\n");
  printf ("#undef FAIL\n");
}

/* Generate the `gen_...' function for a DEFINE_INSN.  */

static void
gen_insn (md_rtx_info *info)
{
  struct pattern_stats stats;
  int i;

  /* See if the pattern for this insn ends with a group of CLOBBERs of (hard)
     registers or MATCH_SCRATCHes.  If so, store away the information for
     later.  */

  rtx insn = info->def;
  if (XVEC (insn, 1))
    {
      int has_hard_reg = 0;

      for (i = XVECLEN (insn, 1) - 1; i > 0; i--)
	{
	  if (GET_CODE (XVECEXP (insn, 1, i)) != CLOBBER)
	    break;

	  if (REG_P (XEXP (XVECEXP (insn, 1, i), 0)))
	    has_hard_reg = 1;
	  else if (GET_CODE (XEXP (XVECEXP (insn, 1, i), 0)) != MATCH_SCRATCH)
	    break;
	}

      if (i != XVECLEN (insn, 1) - 1)
	{
	  struct clobber_pat *p;
	  struct clobber_ent *link = XNEW (struct clobber_ent);
	  int j;

	  link->code_number = info->index;

	  /* See if any previous CLOBBER_LIST entry is the same as this
	     one.  */

	  for (p = clobber_list; p; p = p->next)
	    {
	      if (p->first_clobber != i + 1
		  || XVECLEN (p->pattern, 1) != XVECLEN (insn, 1))
		continue;

	      for (j = i + 1; j < XVECLEN (insn, 1); j++)
		{
		  rtx old_rtx = XEXP (XVECEXP (p->pattern, 1, j), 0);
		  rtx new_rtx = XEXP (XVECEXP (insn, 1, j), 0);

		  /* OLD and NEW_INSN are the same if both are to be a SCRATCH
		     of the same mode,
		     or if both are registers of the same mode and number.  */
		  if (! (GET_CODE (old_rtx) == GET_CODE (new_rtx)
			 && GET_MODE (old_rtx) == GET_MODE (new_rtx)
			 && ((GET_CODE (old_rtx) == MATCH_SCRATCH
			      && GET_CODE (new_rtx) == MATCH_SCRATCH)
			     || (REG_P (old_rtx) && REG_P (new_rtx)
				 && REGNO (old_rtx) == REGNO (new_rtx)))))
		    break;
		}

	      if (j == XVECLEN (insn, 1))
		break;
	    }

	  if (p == 0)
	    {
	      p = XNEW (struct clobber_pat);

	      p->insns = 0;
	      p->pattern = insn;
	      p->first_clobber = i + 1;
	      p->next = clobber_list;
	      p->has_hard_reg = has_hard_reg;
	      clobber_list = p;
	    }

	  link->next = p->insns;
	  p->insns = link;
	}
    }

  /* Don't mention instructions whose names are the null string
     or begin with '*'.  They are in the machine description just
     to be recognized.  */
  if (XSTR (insn, 0)[0] == 0 || XSTR (insn, 0)[0] == '*')
    return;

  printf ("/* %s:%d */\n", info->loc.filename, info->loc.lineno);

  /* Find out how many operands this function has.  */
  get_pattern_stats (&stats, XVEC (insn, 1));
  if (stats.max_dup_opno > stats.max_opno)
    fatal_at (info->loc, "match_dup operand number has no match_operand");

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (insn, 0));
  if (stats.num_generator_args)
    for (i = 0; i < stats.num_generator_args; i++)
      if (i)
	printf (",\n\trtx operand%d ATTRIBUTE_UNUSED", i);
      else
	printf ("rtx operand%d ATTRIBUTE_UNUSED", i);
  else
    printf ("void");
  printf (")\n");
  printf ("{\n");

  /* Output code to construct and return the rtl for the instruction body.  */

  rtx pattern = add_implicit_parallel (XVEC (insn, 1));
  /* ??? This is the traditional behavior, but seems suspect.  */
  char *used = (XVECLEN (insn, 1) == 1
		? NULL
		: XCNEWVEC (char, stats.num_generator_args));
  printf ("  return ");
  gen_exp (pattern, DEFINE_INSN, used, info);
  printf (";\n}\n\n");
  XDELETEVEC (used);
}

/* Generate the `gen_...' function for a DEFINE_EXPAND.  */

static void
gen_expand (md_rtx_info *info)
{
  struct pattern_stats stats;
  int i;
  char *used;

  rtx expand = info->def;
  if (strlen (XSTR (expand, 0)) == 0)
    fatal_at (info->loc, "define_expand lacks a name");
  if (XVEC (expand, 1) == 0)
    fatal_at (info->loc, "define_expand for %s lacks a pattern",
	      XSTR (expand, 0));

  /* Find out how many operands this function has.  */
  get_pattern_stats (&stats, XVEC (expand, 1));
  if (stats.min_scratch_opno != -1
      && stats.min_scratch_opno <= MAX (stats.max_opno, stats.max_dup_opno))
    fatal_at (info->loc, "define_expand for %s needs to have match_scratch "
			 "numbers above all other operands", XSTR (expand, 0));

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (expand, 0));
  if (stats.num_generator_args)
    for (i = 0; i < stats.num_generator_args; i++)
      if (i)
	printf (",\n\trtx operand%d", i);
      else
	printf ("rtx operand%d", i);
  else
    printf ("void");
  printf (")\n");
  printf ("{\n");

  /* If we don't have any C code to write, only one insn is being written,
     and no MATCH_DUPs are present, we can just return the desired insn
     like we do for a DEFINE_INSN.  This saves memory.  */
  if ((XSTR (expand, 3) == 0 || *XSTR (expand, 3) == '\0')
      && stats.max_opno >= stats.max_dup_opno
      && XVECLEN (expand, 1) == 1)
    {
      printf ("  return ");
      gen_exp (XVECEXP (expand, 1, 0), DEFINE_EXPAND, NULL, info);
      printf (";\n}\n\n");
      return;
    }

  /* For each operand referred to only with MATCH_DUPs,
     make a local variable.  */
  for (i = stats.num_generator_args; i <= stats.max_dup_opno; i++)
    printf ("  rtx operand%d;\n", i);
  printf ("  rtx_insn *_val = 0;\n");
  printf ("  start_sequence ();\n");

  /* The fourth operand of DEFINE_EXPAND is some code to be executed
     before the actual construction.
     This code expects to refer to `operands'
     just as the output-code in a DEFINE_INSN does,
     but here `operands' is an automatic array.
     So copy the operand values there before executing it.  */
  if (XSTR (expand, 3) && *XSTR (expand, 3))
    {
      printf ("  {\n");
      if (stats.num_operand_vars > 0)
	printf ("    rtx operands[%d];\n", stats.num_operand_vars);

      /* Output code to copy the arguments into `operands'.  */
      for (i = 0; i < stats.num_generator_args; i++)
	printf ("    operands[%d] = operand%d;\n", i, i);

      /* Output the special code to be executed before the sequence
	 is generated.  */
      optab_pattern p;
      bool can_fail_p = true;
      if (find_optab (&p, XSTR (expand, 0)))
	{
	  gcc_assert (p.op < NUM_OPTABS);
	  if (nofail_optabs[p.op])
	    can_fail_p = false;
	}
      emit_c_code (XSTR (expand, 3), can_fail_p, XSTR (expand, 0));

      /* Output code to copy the arguments back out of `operands'
	 (unless we aren't going to use them at all).  */
      if (XVEC (expand, 1) != 0)
	{
	  for (i = 0; i <= MAX (stats.max_opno, stats.max_dup_opno); i++)
	    {
	      printf ("    operand%d = operands[%d];\n", i, i);
	      printf ("    (void) operand%d;\n", i);
	    }
	}
      printf ("  }\n");
    }

  used = XCNEWVEC (char, stats.num_operand_vars);
  gen_emit_seq (XVEC (expand, 1), used, info);
  XDELETEVEC (used);

  /* Call `get_insns' to extract the list of all the
     insns emitted within this gen_... function.  */

  printf ("  _val = get_insns ();\n");
  printf ("  end_sequence ();\n");
  printf ("  return _val;\n}\n\n");
}

/* Like gen_expand, but generates insns resulting from splitting SPLIT.  */

static void
gen_split (md_rtx_info *info)
{
  struct pattern_stats stats;
  int i;
  rtx split = info->def;
  const char *const name =
    ((GET_CODE (split) == DEFINE_PEEPHOLE2) ? "peephole2" : "split");
  const char *unused;
  char *used;

  if (XVEC (split, 0) == 0)
    fatal_at (info->loc, "%s lacks a pattern",
	      GET_RTX_NAME (GET_CODE (split)));
  else if (XVEC (split, 2) == 0)
    fatal_at (info->loc, "%s lacks a replacement pattern",
	      GET_RTX_NAME (GET_CODE (split)));

  /* Find out how many operands this function has.  */

  get_pattern_stats (&stats, XVEC (split, 2));
  unused = (stats.num_operand_vars == 0 ? " ATTRIBUTE_UNUSED" : "");
  used = XCNEWVEC (char, stats.num_operand_vars);

  /* Output the prototype, function name and argument declarations.  */
  if (GET_CODE (split) == DEFINE_PEEPHOLE2)
    {
      printf ("extern rtx_insn *gen_%s_%d (rtx_insn *, rtx *);\n",
	      name, info->index);
      printf ("rtx_insn *\ngen_%s_%d (rtx_insn *curr_insn ATTRIBUTE_UNUSED,"
	      " rtx *operands%s)\n",
	      name, info->index, unused);
    }
  else
    {
      printf ("extern rtx_insn *gen_split_%d (rtx_insn *, rtx *);\n",
	      info->index);
      printf ("rtx_insn *\ngen_split_%d "
	      "(rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands%s)\n",
	      info->index, unused);
    }
  printf ("{\n");

  /* Declare all local variables.  */
  for (i = 0; i < stats.num_operand_vars; i++)
    printf ("  rtx operand%d;\n", i);
  printf ("  rtx_insn *_val = NULL;\n");

  if (GET_CODE (split) == DEFINE_PEEPHOLE2)
    output_peephole2_scratches (split);

  const char *fn = info->loc.filename;
  for (const char *p = fn; *p; p++)
    if (*p == '/')
      fn = p + 1;

  printf ("  if (dump_file)\n");
  printf ("    fprintf (dump_file, \"Splitting with gen_%s_%d (%s:%d)\\n\");\n",
	  name, info->index, fn, info->loc.lineno);

  printf ("  start_sequence ();\n");

  /* The fourth operand of DEFINE_SPLIT is some code to be executed
     before the actual construction.  */

  if (XSTR (split, 3))
    emit_c_code (XSTR (split, 3), true, name);

  /* Output code to copy the arguments back out of `operands'  */
  for (i = 0; i < stats.num_operand_vars; i++)
    {
      printf ("  operand%d = operands[%d];\n", i, i);
      printf ("  (void) operand%d;\n", i);
    }

  gen_emit_seq (XVEC (split, 2), used, info);

  /* Call `get_insns' to make a list of all the
     insns emitted within this gen_... function.  */

  printf ("  _val = get_insns ();\n");
  printf ("  end_sequence ();\n");
  printf ("  return _val;\n}\n\n");

  free (used);
}

/* Write a function, `add_clobbers', that is given a PARALLEL of sufficient
   size for the insn and an INSN_CODE, and inserts the required CLOBBERs at
   the end of the vector.  */

static void
output_add_clobbers (md_rtx_info *info)
{
  struct clobber_pat *clobber;
  struct clobber_ent *ent;
  int i;

  printf ("\n\nvoid\nadd_clobbers (rtx pattern ATTRIBUTE_UNUSED, int insn_code_number)\n");
  printf ("{\n");
  printf ("  switch (insn_code_number)\n");
  printf ("    {\n");

  for (clobber = clobber_list; clobber; clobber = clobber->next)
    {
      for (ent = clobber->insns; ent; ent = ent->next)
	printf ("    case %d:\n", ent->code_number);

      for (i = clobber->first_clobber; i < XVECLEN (clobber->pattern, 1); i++)
	{
	  printf ("      XVECEXP (pattern, 0, %d) = ", i);
	  gen_exp (XVECEXP (clobber->pattern, 1, i),
		   GET_CODE (clobber->pattern), NULL, info);
	  printf (";\n");
	}

      printf ("      break;\n\n");
    }

  printf ("    default:\n");
  printf ("      gcc_unreachable ();\n");
  printf ("    }\n");
  printf ("}\n");
}

/* Write a function, `added_clobbers_hard_reg_p' that is given an insn_code
   number that will have clobbers added (as indicated by `recog') and returns
   1 if those include a clobber of a hard reg or 0 if all of them just clobber
   SCRATCH.  */

static void
output_added_clobbers_hard_reg_p (void)
{
  struct clobber_pat *clobber;
  struct clobber_ent *ent;
  int clobber_p, used;

  printf ("\n\nint\nadded_clobbers_hard_reg_p (int insn_code_number)\n");
  printf ("{\n");
  printf ("  switch (insn_code_number)\n");
  printf ("    {\n");

  for (clobber_p = 0; clobber_p <= 1; clobber_p++)
    {
      used = 0;
      for (clobber = clobber_list; clobber; clobber = clobber->next)
	if (clobber->has_hard_reg == clobber_p)
	  for (ent = clobber->insns; ent; ent = ent->next)
	    {
	      printf ("    case %d:\n", ent->code_number);
	      used++;
	    }

      if (used)
	printf ("      return %d;\n\n", clobber_p);
    }

  printf ("    default:\n");
  printf ("      gcc_unreachable ();\n");
  printf ("    }\n");
  printf ("}\n");
}

/* Generate code to invoke find_free_register () as needed for the
   scratch registers used by the peephole2 pattern in SPLIT.  */

static void
output_peephole2_scratches (rtx split)
{
  int i;
  int insn_nr = 0;
  bool first = true;

  for (i = 0; i < XVECLEN (split, 0); i++)
    {
      rtx elt = XVECEXP (split, 0, i);
      if (GET_CODE (elt) == MATCH_SCRATCH)
	{
	  int last_insn_nr = insn_nr;
	  int cur_insn_nr = insn_nr;
	  int j;
	  for (j = i + 1; j < XVECLEN (split, 0); j++)
	    if (GET_CODE (XVECEXP (split, 0, j)) == MATCH_DUP)
	      {
		if (XINT (XVECEXP (split, 0, j), 0) == XINT (elt, 0))
		  last_insn_nr = cur_insn_nr;
	      }
	    else if (GET_CODE (XVECEXP (split, 0, j)) != MATCH_SCRATCH)
	      cur_insn_nr++;

	  if (first)
	    {
	      printf ("  HARD_REG_SET _regs_allocated;\n");
	      printf ("  CLEAR_HARD_REG_SET (_regs_allocated);\n");
	      first = false;
	    }

	  printf ("  if ((operands[%d] = peep2_find_free_register (%d, %d, \"%s\", %smode, &_regs_allocated)) == NULL_RTX)\n\
    return NULL;\n",
		  XINT (elt, 0),
		  insn_nr, last_insn_nr,
		  XSTR (elt, 1),
		  GET_MODE_NAME (GET_MODE (elt)));

	}
      else if (GET_CODE (elt) != MATCH_DUP)
	insn_nr++;
    }
}

/* Print "arg<N>" parameter declarations for each argument N of ONAME.  */

static void
print_overload_arguments (overloaded_name *oname)
{
  for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
    printf ("%s%s arg%d", i == 0 ? "" : ", ", oname->arg_types[i], i);
}

/* Print code to test whether INSTANCE should be chosen, given that
   argument N of the overload is available as "arg<N>".  */

static void
print_overload_test (overloaded_instance *instance)
{
  for (unsigned int i = 0; i < instance->arg_values.length (); ++i)
    printf ("%sarg%d == %s", i == 0 ? "  if (" : "\n      && ",
	    i, instance->arg_values[i]);
  printf (")\n");
}

/* Emit a maybe_code_for_* function for ONAME.  */

static void
handle_overloaded_code_for (overloaded_name *oname)
{
  /* Print the function prototype.  */
  printf ("\ninsn_code\nmaybe_code_for_%s (", oname->name);
  print_overload_arguments (oname);
  printf (")\n{\n");

  /* Use a sequence of "if" statements for each instance.  */
  for (overloaded_instance *instance = oname->first_instance;
       instance; instance = instance->next)
    {
      print_overload_test (instance);
      printf ("    return CODE_FOR_%s;\n", instance->name);
    }

  /* Return null if no match was found.  */
  printf ("  return CODE_FOR_nothing;\n}\n");
}

/* Emit a maybe_gen_* function for ONAME.  */

static void
handle_overloaded_gen (overloaded_name *oname)
{
  unsigned HOST_WIDE_INT seen = 0;
  /* All patterns must have the same number of operands.  */
  for (overloaded_instance *instance = oname->first_instance->next;
       instance; instance = instance->next)
    {
      pattern_stats stats;
      get_pattern_stats (&stats, XVEC (instance->insn, 1));
      unsigned HOST_WIDE_INT mask
	= HOST_WIDE_INT_1U << stats.num_generator_args;
      if (seen & mask)
	continue;

      seen |= mask;

      /* Print the function prototype.  */
      printf ("\nrtx\nmaybe_gen_%s (", oname->name);
      print_overload_arguments (oname);
      for (int i = 0; i < stats.num_generator_args; ++i)
	printf (", rtx x%d", i);
      printf (")\n{\n");

      /* Use maybe_code_for_*, instead of duplicating the selection
	 logic here.  */
      printf ("  insn_code code = maybe_code_for_%s (", oname->name);
      for (unsigned int i = 0; i < oname->arg_types.length (); ++i)
	printf ("%sarg%d", i == 0 ? "" : ", ", i);
      printf (");\n"
	      "  if (code != CODE_FOR_nothing)\n"
	      "    {\n"
	      "      gcc_assert (insn_data[code].n_generator_args == %d);\n"
	      "      return GEN_FCN (code) (", stats.num_generator_args);
      for (int i = 0; i < stats.num_generator_args; ++i)
	printf ("%sx%d", i == 0 ? "" : ", ", i);
      printf (");\n"
	      "    }\n"
	      "  else\n"
	      "    return NULL_RTX;\n"
	      "}\n");
    }
}

int
main (int argc, const char **argv)
{
  progname = "genemit";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

#define DEF_INTERNAL_OPTAB_FN(NAME, FLAGS, OPTAB, TYPE) \
  nofail_optabs[OPTAB##_optab] = true;
#include "internal-fn.def"

  /* Assign sequential codes to all entries in the machine description
     in parallel with the tables in insn-output.c.  */

  printf ("/* Generated automatically by the program `genemit'\n\
from the machine description file `md'.  */\n\n");

  printf ("#define IN_TARGET_CODE 1\n");
  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"coretypes.h\"\n");
  printf ("#include \"backend.h\"\n");
  printf ("#include \"predict.h\"\n");
  printf ("#include \"tree.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"alias.h\"\n");
  printf ("#include \"varasm.h\"\n");
  printf ("#include \"stor-layout.h\"\n");
  printf ("#include \"calls.h\"\n");
  printf ("#include \"memmodel.h\"\n");
  printf ("#include \"tm_p.h\"\n");
  printf ("#include \"flags.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"expmed.h\"\n");
  printf ("#include \"dojump.h\"\n");
  printf ("#include \"explow.h\"\n");
  printf ("#include \"emit-rtl.h\"\n");
  printf ("#include \"stmt.h\"\n");
  printf ("#include \"expr.h\"\n");
  printf ("#include \"insn-codes.h\"\n");
  printf ("#include \"optabs.h\"\n");
  printf ("#include \"dfp.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"df.h\"\n");
  printf ("#include \"resource.h\"\n");
  printf ("#include \"reload.h\"\n");
  printf ("#include \"diagnostic-core.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"tm-constrs.h\"\n");
  printf ("#include \"ggc.h\"\n");
  printf ("#include \"target.h\"\n\n");

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
	gen_insn (&info);
	break;

      case DEFINE_EXPAND:
	printf ("/* %s:%d */\n", info.loc.filename, info.loc.lineno);
	gen_expand (&info);
	break;

      case DEFINE_SPLIT:
	printf ("/* %s:%d */\n", info.loc.filename, info.loc.lineno);
	gen_split (&info);
	break;

      case DEFINE_PEEPHOLE2:
	printf ("/* %s:%d */\n", info.loc.filename, info.loc.lineno);
	gen_split (&info);
	break;

      default:
	break;
      }

  /* Write out the routines to add CLOBBERs to a pattern and say whether they
     clobber a hard reg.  */
  output_add_clobbers (&info);
  output_added_clobbers_hard_reg_p ();

  for (overloaded_name *oname = rtx_reader_ptr->get_overloads ();
       oname; oname = oname->next)
    {
      handle_overloaded_code_for (oname);
      handle_overloaded_gen (oname);
    }

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
