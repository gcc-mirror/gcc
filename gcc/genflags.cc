/* Generate from machine description:
   - some flags HAVE_... saying which simple standard instructions are
   available for this machine.
   Copyright (C) 1987-2026 Free Software Foundation, Inc.

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
#include "obstack.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"

/* Obstack to remember insns with.  */
static struct obstack obstack;

/* Max size of names encountered.  */
static int max_id_len;

/* Max operand encountered in a scan over some insn.  */
static int max_opno;

static void max_operand_1 (rtx);
static int num_operands (rtx);
static void gen_proto (rtx);

/* Count the number of match_operand's found.  */

static void
max_operand_1 (rtx x)
{
  RTX_CODE code;
  int i;
  int len;
  const char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  if (code == MATCH_OPERAND || code == MATCH_OPERATOR
      || code == MATCH_PARALLEL)
    max_opno = MAX (max_opno, XINT (x, 0));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	max_operand_1 (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    max_operand_1 (XVECEXP (x, i, j));
	}
    }
}

static int
num_operands (rtx insn)
{
  int len = XVECLEN (insn, 1);
  int i;

  max_opno = -1;

  for (i = 0; i < len; i++)
    max_operand_1 (XVECEXP (insn, 1, i));

  return max_opno + 1;
}

/* Print out prototype information for a generator function.  If the
   insn pattern has been elided, print out a dummy generator that
   does nothing.  */

static void
gen_proto (rtx insn)
{
  int num = num_operands (insn);
  int i;
  const char *name = XSTR (insn, 0);
  int truth = maybe_eval_c_test (XSTR (insn, 2));
  pattern_stats stats;
  get_pattern_stats (&stats, XVEC (insn, 1));
  /* DEFINE_INSN, and a one-element define_expand with no C code,
     return the pattern.  An expand that emits insns returns an
     rtx_insn *.  */
  bool return_pat = (GET_CODE (insn) == DEFINE_INSN
		     || ((!XSTR (insn, 3) || XSTR (insn, 3)[0] == '\0')
			 && stats.max_opno >= stats.max_dup_opno
			 && XVECLEN (insn, 1) == 1));
  const char *ret_type = return_pat ? "rtx" : "rtx_insn *";

  if (truth != 0)
    printf ("extern %s gen_%-*s (", ret_type, max_id_len, name);
  else
    printf ("static inline %s gen_%-*s (", ret_type, max_id_len,
	     name);

  if (num == 0)
    fputs ("void", stdout);
  else
    {
      for (i = 1; i < num; i++)
	fputs ("rtx, ", stdout);

      fputs ("rtx", stdout);
    }

  puts (");");

  if (return_pat)
    {
      /* genemit drops an elided pattern, so the wrapper is a stub.  */
      if (truth == 0)
	{
	  printf ("static inline rtx_insn *\ngen_insn_%s", name);
	  if (num > 0)
	    {
	      putchar ('(');
	      for (i = 0; i < num - 1; i++)
		printf ("rtx ARG_UNUSED (%c), ", 'a' + i);
	      printf ("rtx ARG_UNUSED (%c))\n", 'a' + i);
	    }
	  else
	    puts ("(void)");
	  puts ("{\n  return 0;\n}");
	}
      else
	{
	  printf ("extern rtx_insn *gen_insn_%s (", name);
	  if (num > 0)
	    {
	      for (i = 1; i < num; i++)
		printf ("rtx, ");
	      printf ("rtx");
	    }
	  puts (");");
	}
    }
  else
    {
      printf ("static inline rtx_insn *\ngen_insn_%s (", name);
      if (num > 0)
	{
	  for (i = 1; i < num; i++)
	    printf ("rtx %c, ", 'a' + i - 1);
	  printf ("rtx %c", 'a' + i - 1);
	}
      printf (")\n{ return gen_%s (", name);
      if (num > 0)
	{
	  for (i = 1; i < num; i++)
	    printf ("%c, ", 'a' + i - 1);
	  printf ("%c", 'a' + i - 1);
	}
      printf ("); }\n");
    }

  /* Some back ends want to take the address of generator functions,
     so we cannot simply use #define for these dummy definitions.  */
  if (truth == 0)
    {
      printf ("static inline %s\ngen_%s", ret_type, name);
      if (num > 0)
	{
	  putchar ('(');
	  for (i = 0; i < num-1; i++)
	    printf ("rtx ARG_UNUSED (%c), ", 'a' + i);
	  printf ("rtx ARG_UNUSED (%c))\n", 'a' + i);
	}
      else
	puts ("(void)");
      puts ("{\n  return 0;\n}");
    }

}

static void
gen_insn (md_rtx_info *info)
{
  rtx insn = info->def;
  const char *name = XSTR (insn, 0);
  const char *p;
  const char *lt, *gt;
  int len;
  int truth = maybe_eval_c_test (XSTR (insn, 2));

  lt = strchr (name, '<');
  if (lt && strchr (lt + 1, '>'))
    {
      error_at (info->loc, "unresolved iterator in %s", name);
      return;
    }

  gt = strchr (name, '>');
  if (lt || gt)
    {
      error_at (info->loc, "unmatched angle brackets, likely "
		"an error in iterator syntax in %s", name);
      return;
    }

  /* Don't mention instructions whose names are the null string
     or begin with '*'.  They are in the machine description just
     to be recognized.  */
  if (name[0] == 0 || name[0] == '*')
    return;

  len = strlen (name);

  if (len > max_id_len)
    max_id_len = len;

  if (truth == 0)
    /* Emit nothing.  */;
  else if (truth == 1)
    printf ("#define HAVE_%s 1\n", name);
  else
    {
      /* Write the macro definition, putting \'s at the end of each line,
	 if more than one.  */
      printf ("#define HAVE_%s (", name);
      for (p = XSTR (insn, 2); *p; p++)
	{
	  if (IS_VSPACE (*p))
	    fputs (" \\\n", stdout);
	  else
	    putchar (*p);
	}
      fputs (")\n", stdout);
    }

  obstack_grow (&obstack, &insn, sizeof (rtx));
}

int
main (int argc, const char **argv)
{
  rtx dummy;
  rtx *insns;
  rtx *insn_ptr;

  progname = "genflags";
  obstack_init (&obstack);

  /* We need to see all the possibilities.  Elided insns may have
     direct calls to their generators in C code.  */
  insn_elision = 0;

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `genflags'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_FLAGS_H");
  puts ("#define GCC_INSN_FLAGS_H\n");

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
      case DEFINE_EXPAND:
	gen_insn (&info);
	break;

      default:
	break;
      }

  /* Print out the prototypes now.  */
  dummy = (rtx) 0;
  obstack_grow (&obstack, &dummy, sizeof (rtx));
  insns = XOBFINISH (&obstack, rtx *);

  for (insn_ptr = insns; *insn_ptr; insn_ptr++)
    gen_proto (*insn_ptr);

  puts ("\n#endif /* GCC_INSN_FLAGS_H */");

  if (have_error || ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
