/* Process machine description and calculate constant conditions.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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

/* In a machine description, all of the insn patterns - define_insn,
   define_expand, define_split, define_peephole, define_peephole2 -
   contain an optional C expression which makes the final decision
   about whether or not this pattern is usable.  That expression may
   turn out to be always false when the compiler is built.  If it is,
   most of the programs that generate code from the machine
   description can simply ignore the entire pattern.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "errors.h"
#include "hashtab.h"
#include "gensupport.h"

/* so we can include except.h in the generated file */
static int saw_eh_return;

static htab_t condition_table;

static void add_condition	PARAMS ((const char *));
static void write_header	PARAMS ((void));
static void write_conditions	PARAMS ((void));
static int write_one_condition	PARAMS ((PTR *, PTR));

extern int main			PARAMS ((int, char **));

/* Record the C test expression EXPR in the condition_table.
   Duplicates clobber previous entries, which leaks memory, but
   we don't care for this application.  */

static void
add_condition (expr)
     const char *expr;
{
  struct c_test *test;

  if (expr[0] == 0)
    return;

  test = (struct c_test *) xmalloc (sizeof (struct c_test));
  test->expr = expr;

  *(htab_find_slot (condition_table, test, INSERT)) = test;
}

/* Generate the header for insn-conditions.c.  */

static void
write_header ()
{
  puts ("\
/* Generated automatically by the program `genconditions' from the target\n\
   machine description file.  */\n\
\n\
#include \"hconfig.h\"\n\
#include \"insn-constants.h\"\n");

  puts ("\
/* Do not allow checking to confuse the issue.  */\n\
#undef ENABLE_CHECKING\n\
#undef ENABLE_TREE_CHECKING\n\
#undef ENABLE_RTL_CHECKING\n\
#undef ENABLE_RTL_FLAG_CHECKING\n\
#undef ENABLE_GC_CHECKING\n\
#undef ENABLE_GC_ALWAYS_COLLECT\n");

  puts ("\
#include \"system.h\"\n\
#include \"rtl.h\"\n\
#include \"tm_p.h\"\n\
#include \"function.h\"\n");

  puts ("\
/* Fake - insn-config.h doesn't exist yet.  */\n\
#define MAX_RECOG_OPERANDS 10\n\
#define MAX_DUP_OPERANDS 10\n\
#define MAX_INSNS_PER_SPLIT 5\n");

  puts ("\
#include \"regs.h\"\n\
#include \"recog.h\"\n\
#include \"real.h\"\n\
#include \"output.h\"\n\
#include \"flags.h\"\n\
#include \"hard-reg-set.h\"\n\
#include \"resource.h\"\n\
#include \"toplev.h\"\n\
#include \"reload.h\"\n\
#include \"gensupport.h\"\n");

  if (saw_eh_return)
    puts ("#define HAVE_eh_return 1");
  puts ("#include \"except.h\"\n");

  puts ("\
/* Dummy external declarations.  */\n\
extern rtx insn;\n\
extern rtx ins1;\n\
extern rtx operands[];\n\
extern int next_insn_tests_no_inequality PARAMS ((rtx));\n");

  puts ("\
/* If we don't have __builtin_constant_p, or it's not acceptable in\n\
   array initializers, fall back to assuming that all conditions\n\
   potentially vary at run time.  It works in 3.0.1 and later; 3.0\n\
   only when not optimizing.  */\n\
#if (GCC_VERSION >= 3001) || ((GCC_VERSION == 3000) && !__OPTIMIZE__)\n\
# define MAYBE_EVAL(expr) (__builtin_constant_p(expr) ? (int) (expr) : -1)\n\
#else\n\
# define MAYBE_EVAL(expr) -1\n\
#endif\n");
}

/* Write out one entry in the conditions table, using the data pointed
   to by SLOT.  Each entry looks like this:
  { "! optimize_size && ! TARGET_READ_MODIFY_WRITE",
    MAYBE_EVAL (! optimize_size && ! TARGET_READ_MODIFY_WRITE) },  */

static int
write_one_condition (slot, dummy)
     PTR *slot;
     PTR dummy ATTRIBUTE_UNUSED;
{
  const struct c_test *test = * (const struct c_test **) slot;
  const char *p;

  fputs ("  { \"", stdout);
  for (p = test->expr; *p; p++)
    {
      if (*p == '\n')
	fputs ("\\n\\\n", stdout);
      else if (*p == '"')
	fputs ("\\\"", stdout);
      else
	putchar (*p);
    }

  printf ("\",\n    MAYBE_EVAL (%s) },\n", test->expr);
  return 1;
}

/* Write out the complete conditions table, its size, and a flag
   indicating that gensupport.c can now do insn elision.  */
static void
write_conditions ()
{
  puts ("\
/* This table lists each condition found in the machine description.\n\
   Each condition is mapped to its truth value (0 or 1), or -1 if that\n\
   cannot be calculated at compile time. */\n\
\n\
const struct c_test insn_conditions[] = {");

  htab_traverse (condition_table, write_one_condition, 0);

  puts ("};\n");

  printf ("const size_t n_insn_conditions = %lu;\n",
	  (unsigned long) htab_elements (condition_table));
  puts ("const int insn_elision_unavailable = 0;");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  int pattern_lineno; /* not used */
  int code;

  progname = "genconditions";

  if (argc <= 1)
    fatal ("No input file name.");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  condition_table = htab_create (1000, hash_c_test, cmp_c_test, NULL);

  /* Read the machine description.  */

  while (1)
    {
      desc = read_md_rtx (&pattern_lineno, &code);
      if (desc == NULL)
	break;

      /* N.B. define_insn_and_split, define_cond_exec are handled
	 entirely within read_md_rtx; we never see them.  */
      switch (GET_CODE (desc))
	{
	default:
	  break;

	case DEFINE_INSN:
	case DEFINE_EXPAND:
	  add_condition (XSTR (desc, 2));
	  /* except.h needs to know whether there is an eh_return
	     pattern in the machine description.  */
	  if (!strcmp (XSTR (desc, 0), "eh_return"))
	    saw_eh_return = 1;
	  break;

	case DEFINE_SPLIT:
	case DEFINE_PEEPHOLE:
	case DEFINE_PEEPHOLE2:
	  add_condition (XSTR (desc, 1));
	  break;
	}
    }

  write_header ();
  write_conditions ();

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
