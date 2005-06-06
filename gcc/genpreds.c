/* Generate from machine description:
   - prototype declarations for operand predicates (tm-preds.h)
   - function definitions of operand predicates, if defined new-style
     (insn-preds.c)
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "gensupport.h"
#include "obstack.h"

/* The new way to declare predicates is with (define_predicate) or
   (define_special_predicate) expressions in the machine description.
   This provides a function body as well as a name.  */
static void
process_define_predicate (rtx defn)
{
  struct pred_data *pred;
  if (XEXP (defn, 1) == 0)
    {
      error ("%s: must give a predicate expression", XSTR (defn, 0));
      return;
    }

  pred = xcalloc (sizeof (struct pred_data), 1);
  pred->name    = XSTR (defn, 0);
  pred->exp     = XEXP (defn, 1);
  pred->c_block = XSTR (defn, 2);

  if (GET_CODE (defn) == DEFINE_SPECIAL_PREDICATE)
    pred->special = true;

  add_predicate (pred);
}

/* Write tm-preds.h.  Unfortunately, it is impossible to forward-declare
   an enumeration in portable C, so we have to condition all these
   prototypes on HAVE_MACHINE_MODES.  */
static void
write_tm_preds_h (void)
{
  struct pred_data *p;

  printf ("\
/* Generated automatically by the program '%s'\n\
   from the machine description file '%s'.  */\n\n", progname, in_fname);

  puts ("\
#ifndef GCC_TM_PREDS_H\n\
#define GCC_TM_PREDS_H\n\
\n\
#ifdef HAVE_MACHINE_MODES");

  FOR_ALL_PREDICATES (p)
    printf ("extern int %s (rtx, enum machine_mode);\n", p->name);

  puts ("\
#endif /* HAVE_MACHINE_MODES */\n\
#endif /* tm-preds.h */");
}

/* Given a predicate, if it has an embedded C block, write the block
   out as a static inline subroutine, and augment the RTL test with a
   match_test that calls that subroutine.  For instance,

       (define_predicate "basereg_operand"
         (match_operand 0 "register_operand")
       {
         if (GET_CODE (op) == SUBREG)
           op = SUBREG_REG (op);
         return REG_POINTER (op);
       })

   becomes

       static inline int basereg_operand_1(rtx op, enum machine_mode mode)
       {
         if (GET_CODE (op) == SUBREG)
           op = SUBREG_REG (op);
         return REG_POINTER (op);
       }

       (define_predicate "basereg_operand"
         (and (match_operand 0 "register_operand")
	      (match_test "basereg_operand_1 (op, mode)")))

   The only wart is that there's no way to insist on a { } string in
   an RTL template, so we have to handle "" strings.  */

   
static void
write_predicate_subfunction (struct pred_data *p)
{
  const char *match_test_str;
  rtx match_test_exp, and_exp;

  if (p->c_block[0] == '\0')
    return;

  /* Construct the function-call expression.  */
  obstack_grow (rtl_obstack, p->name, strlen (p->name));
  obstack_grow (rtl_obstack, "_1 (op, mode)",
		sizeof "_1 (op, mode)");
  match_test_str = XOBFINISH (rtl_obstack, const char *);

  /* Add the function-call expression to the complete expression to be
     evaluated.  */
  match_test_exp = rtx_alloc (MATCH_TEST);
  XSTR (match_test_exp, 0) = match_test_str;

  and_exp = rtx_alloc (AND);
  XEXP (and_exp, 0) = p->exp;
  XEXP (and_exp, 1) = match_test_exp;

  p->exp = and_exp;

  printf ("static inline int\n"
	  "%s_1 (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)\n",
	  p->name);
  print_rtx_ptr_loc (p->c_block);
  if (p->c_block[0] == '{')
    fputs (p->c_block, stdout);
  else
    printf ("{\n  %s\n}", p->c_block);
  fputs ("\n\n", stdout);
}

/* Given an RTL expression EXP, find all subexpressions which we may
   assume to perform mode tests.  Normal MATCH_OPERAND does;
   MATCH_CODE does if and only if it accepts CONST_INT or
   CONST_DOUBLE; and we have to assume that MATCH_TEST does not.
   These combine in almost-boolean fashion - the only exception is
   that (not X) must be assumed not to perform a mode test, whether or
   not X does.

   The mark is the RTL /v flag, which is true for subexpressions which
   do *not* perform mode tests.
*/
#define NO_MODE_TEST(EXP) RTX_FLAG (EXP, volatil)
static void
mark_mode_tests (rtx exp)
{
  switch (GET_CODE (exp))
    {
    case MATCH_OPERAND:
      {
	struct pred_data *p = lookup_predicate (XSTR (exp, 1));
	if (!p)
	  error ("reference to undefined predicate '%s'", XSTR (exp, 1));
	else if (p->special || GET_MODE (exp) != VOIDmode)
	  NO_MODE_TEST (exp) = 1;
      }
      break;

    case MATCH_CODE:
      if (!strstr (XSTR (exp, 0), "const_int")
	  && !strstr (XSTR (exp, 0), "const_double"))
	NO_MODE_TEST (exp) = 1;
      break;

    case MATCH_TEST:
    case NOT:
      NO_MODE_TEST (exp) = 1;
      break;

    case AND:
      mark_mode_tests (XEXP (exp, 0));
      mark_mode_tests (XEXP (exp, 1));

      NO_MODE_TEST (exp) = (NO_MODE_TEST (XEXP (exp, 0))
			    && NO_MODE_TEST (XEXP (exp, 1)));
      break;
      
    case IOR:
      mark_mode_tests (XEXP (exp, 0));
      mark_mode_tests (XEXP (exp, 1));

      NO_MODE_TEST (exp) = (NO_MODE_TEST (XEXP (exp, 0))
			    || NO_MODE_TEST (XEXP (exp, 1)));
      break;

    case IF_THEN_ELSE:
      /* A ? B : C does a mode test if (one of A and B) does a mode
	 test, and C does too.  */
      mark_mode_tests (XEXP (exp, 0));
      mark_mode_tests (XEXP (exp, 1));
      mark_mode_tests (XEXP (exp, 2));

      NO_MODE_TEST (exp) = ((NO_MODE_TEST (XEXP (exp, 0))
			     && NO_MODE_TEST (XEXP (exp, 1)))
			    || NO_MODE_TEST (XEXP (exp, 2)));
      break;

    default:
      error ("'%s' cannot be used in a define_predicate expression",
	     GET_RTX_NAME (GET_CODE (exp)));
    }
}

/* Given a predicate, work out where in its RTL expression to add
   tests for proper modes.  Special predicates do not get any such
   tests.  We try to avoid adding tests when we don't have to; in
   particular, other normal predicates can be counted on to do it for
   us.  */

static void
add_mode_tests (struct pred_data *p)
{
  rtx match_test_exp, and_exp;
  rtx *pos;

  /* Don't touch special predicates.  */
  if (p->special)
    return;

  mark_mode_tests (p->exp);

  /* If the whole expression already tests the mode, we're done.  */
  if (!NO_MODE_TEST (p->exp))
    return;

  match_test_exp = rtx_alloc (MATCH_TEST);
  XSTR (match_test_exp, 0) = "mode == VOIDmode || GET_MODE (op) == mode";
  and_exp = rtx_alloc (AND);
  XEXP (and_exp, 1) = match_test_exp;

  /* It is always correct to rewrite p->exp as

        (and (...) (match_test "mode == VOIDmode || GET_MODE (op) == mode"))

     but there are a couple forms where we can do better.  If the
     top-level pattern is an IOR, and one of the two branches does test
     the mode, we can wrap just the branch that doesn't.  Likewise, if
     we have an IF_THEN_ELSE, and one side of it tests the mode, we can
     wrap just the side that doesn't.  And, of course, we can repeat this
     descent as many times as it works.  */

  pos = &p->exp;
  for (;;)
    {
      rtx subexp = *pos;

      switch (GET_CODE (subexp))
	{
	case IOR:
	  {
	    int test0 = NO_MODE_TEST (XEXP (subexp, 0));
	    int test1 = NO_MODE_TEST (XEXP (subexp, 1));
	    
	    gcc_assert (test0 || test1);
	    
	    if (test0 && test1)
	      goto break_loop;
	    pos = test0 ? &XEXP (subexp, 0) : &XEXP (subexp, 1);
	  }
	  break;
	  
	case IF_THEN_ELSE:
	  {
	    int test0 = NO_MODE_TEST (XEXP (subexp, 0));
	    int test1 = NO_MODE_TEST (XEXP (subexp, 1));
	    int test2 = NO_MODE_TEST (XEXP (subexp, 2));
	    
	    gcc_assert ((test0 && test1) || test2);
	    
	    if (test0 && test1 && test2)
	      goto break_loop;
	    if (test0 && test1)
	      /* Must put it on the dependent clause, not the
	      	 controlling expression, or we change the meaning of
	      	 the test.  */
	      pos = &XEXP (subexp, 1);
	    else
	      pos = &XEXP (subexp, 2);
	  }
	  break;
	  
	default:
	  goto break_loop;
	}
    }
 break_loop:
  XEXP (and_exp, 0) = *pos;
  *pos = and_exp;
}


/* CODES is a list of RTX codes.  Write out an expression which
   determines whether the operand has one of those codes.  */
static void
write_match_code (const char *codes)
{
  const char *code;

  while ((code = scan_comma_elt (&codes)) != 0)
    {
      fputs ("GET_CODE (op) == ", stdout);
      while (code < codes)
	{
	  putchar (TOUPPER (*code));
	  code++;
	}
      
      if (*codes == ',')
	fputs (" || ", stdout);
    }
}

/* EXP is an RTL (sub)expression for a predicate.  Recursively
   descend the expression and write out an equivalent C expression.  */
static void
write_predicate_expr (const char *name, rtx exp)
{
  switch (GET_CODE (exp))
    {
    case AND:
      putchar ('(');
      write_predicate_expr (name, XEXP (exp, 0));
      fputs (") && (", stdout);
      write_predicate_expr (name, XEXP (exp, 1));
      putchar (')');
      break;
  
    case IOR:
      putchar ('(');
      write_predicate_expr (name, XEXP (exp, 0));
      fputs (") || (", stdout);
      write_predicate_expr (name, XEXP (exp, 1));
      putchar (')');
      break;

    case NOT:
      fputs ("!(", stdout);
      write_predicate_expr (name, XEXP (exp, 0));
      putchar (')');
      break;

    case IF_THEN_ELSE:
      putchar ('(');
      write_predicate_expr (name, XEXP (exp, 0));
      fputs (") ? (", stdout);
      write_predicate_expr (name, XEXP (exp, 1));
      fputs (") : (", stdout);
      write_predicate_expr (name, XEXP (exp, 2));
      putchar (')');
      break;

    case MATCH_OPERAND:
      if (GET_MODE (exp) == VOIDmode)
        printf ("%s (op, mode)", XSTR (exp, 1));
      else
        printf ("%s (op, %smode)", XSTR (exp, 1), mode_name[GET_MODE (exp)]);
      break;

    case MATCH_CODE:
      write_match_code (XSTR (exp, 0));
      break;

    case MATCH_TEST:
      print_c_condition (XSTR (exp, 0));
      break;

    default:
      error ("%s: cannot use '%s' in a predicate expression",
	     name, GET_RTX_NAME (GET_CODE (exp)));
      putchar ('0');
    }
}

/* Given a predicate, write out a complete C function to compute it.  */
static void
write_one_predicate_function (struct pred_data *p)
{
  if (!p->exp)
    return;

  write_predicate_subfunction (p);
  add_mode_tests (p);

  /* A normal predicate can legitimately not look at enum machine_mode
     if it accepts only CONST_INTs and/or CONST_DOUBLEs.  */
  printf ("int\n%s (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)\n"
	  "{\n  return ",
	  p->name);
  write_predicate_expr (p->name, p->exp);
  fputs (";\n}\n\n", stdout);
}

/* Write insn-preds.c.  
   N.B. the list of headers to include was copied from genrecog; it
   may not be ideal.

   FUTURE: Write #line markers referring back to the machine
   description.  (Can't practically do this now since we don't know
   the line number of the C block - just the line number of the enclosing
   expression.)  */
static void
write_insn_preds_c (void)
{
  struct pred_data *p;

  printf ("\
/* Generated automatically by the program '%s'\n\
   from the machine description file '%s'.  */\n\n", progname, in_fname);

  puts ("\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"\n\
#include \"tm.h\"\n\
#include \"rtl.h\"\n\
#include \"tree.h\"\n\
#include \"tm_p.h\"\n\
#include \"function.h\"\n\
#include \"insn-config.h\"\n\
#include \"recog.h\"\n\
#include \"real.h\"\n\
#include \"output.h\"\n\
#include \"flags.h\"\n\
#include \"hard-reg-set.h\"\n\
#include \"resource.h\"\n\
#include \"toplev.h\"\n\
#include \"reload.h\"\n\
#include \"regs.h\"\n");

  FOR_ALL_PREDICATES (p)
    write_one_predicate_function (p);
}

/* Argument parsing.  */
static bool gen_header;
static bool
parse_option (const char *opt)
{
  if (!strcmp (opt, "-h"))
    {
      gen_header = true;
      return 1;
    }
  else
    return 0;
}

/* Master control.  */
int
main (int argc, char **argv)
{
  rtx defn;
  int pattern_lineno, next_insn_code = 0;

  progname = argv[0];
  if (argc <= 1)
    fatal ("no input file name");
  if (init_md_reader_args_cb (argc, argv, parse_option) != SUCCESS_EXIT_CODE)
    return FATAL_EXIT_CODE;

  while ((defn = read_md_rtx (&pattern_lineno, &next_insn_code)) != 0)
    {
      if (GET_CODE (defn) == DEFINE_PREDICATE
	  || GET_CODE (defn) == DEFINE_SPECIAL_PREDICATE)
	process_define_predicate (defn);
    }

  if (gen_header)
    write_tm_preds_h ();
  else
    write_insn_preds_c ();

  if (have_error || ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}

/* Dummy for debugging purposes.  */
const char *
get_insn_name (int code ATTRIBUTE_UNUSED)
{
  return 0;
}
