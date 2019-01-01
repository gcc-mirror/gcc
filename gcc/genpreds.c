/* Generate from machine description:
   - prototype declarations for operand predicates (tm-preds.h)
   - function definitions of operand predicates, if defined new-style
     (insn-preds.c)
   Copyright (C) 2001-2019 Free Software Foundation, Inc.

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

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "obstack.h"
#include "read-md.h"
#include "gensupport.h"

static char general_mem[] = { TARGET_MEM_CONSTRAINT, 0 };

/* Given a predicate expression EXP, from form NAME at location LOC,
   verify that it does not contain any RTL constructs which are not
   valid in predicate definitions.  Returns true if EXP is
   INvalid; issues error messages, caller need not.  */
static bool
validate_exp (rtx exp, const char *name, file_location loc)
{
  if (exp == 0)
    {
      message_at (loc, "%s: must give a predicate expression", name);
      return true;
    }

  switch (GET_CODE (exp))
    {
      /* Ternary, binary, unary expressions: recurse into subexpressions.  */
    case IF_THEN_ELSE:
      if (validate_exp (XEXP (exp, 2), name, loc))
	return true;
      /* fall through */
    case AND:
    case IOR:
      if (validate_exp (XEXP (exp, 1), name, loc))
	return true;
      /* fall through */
    case NOT:
      return validate_exp (XEXP (exp, 0), name, loc);

      /* MATCH_CODE might have a syntax error in its path expression.  */
    case MATCH_CODE:
      {
	const char *p;
	for (p = XSTR (exp, 1); *p; p++)
	  {
	    if (!ISDIGIT (*p) && !ISLOWER (*p))
	      {
		error_at (loc, "%s: invalid character in path "
			  "string '%s'", name, XSTR (exp, 1));
		return true;
	      }
	  }
      }
      gcc_fallthrough ();

      /* These need no special checking.  */
    case MATCH_OPERAND:
    case MATCH_TEST:
      return false;

    default:
      error_at (loc, "%s: cannot use '%s' in a predicate expression",
		name, GET_RTX_NAME (GET_CODE (exp)));
      return true;
    }
}

/* Predicates are defined with (define_predicate) or
   (define_special_predicate) expressions in the machine description.  */
static void
process_define_predicate (md_rtx_info *info)
{
  validate_exp (XEXP (info->def, 1), XSTR (info->def, 0), info->loc);
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

       static inline int basereg_operand_1(rtx op, machine_mode mode)
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
	  "%s_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)\n",
	  p->name);
  rtx_reader_ptr->print_md_ptr_loc (p->c_block);
  if (p->c_block[0] == '{')
    fputs (p->c_block, stdout);
  else
    printf ("{\n  %s\n}", p->c_block);
  fputs ("\n\n", stdout);
}

/* Given a predicate expression EXP, from form NAME, determine whether
   it refers to the variable given as VAR.  */
static bool
needs_variable (rtx exp, const char *var)
{
  switch (GET_CODE (exp))
    {
      /* Ternary, binary, unary expressions need a variable if
	 any of their subexpressions do.  */
    case IF_THEN_ELSE:
      if (needs_variable (XEXP (exp, 2), var))
	return true;
      /* fall through */
    case AND:
    case IOR:
      if (needs_variable (XEXP (exp, 1), var))
	return true;
      /* fall through */
    case NOT:
      return needs_variable (XEXP (exp, 0), var);

      /* MATCH_CODE uses "op", but nothing else.  */
    case MATCH_CODE:
      return !strcmp (var, "op");

      /* MATCH_OPERAND uses "op" and may use "mode".  */
    case MATCH_OPERAND:
      if (!strcmp (var, "op"))
	return true;
      if (!strcmp (var, "mode") && GET_MODE (exp) == VOIDmode)
	return true;
      return false;

      /* MATCH_TEST uses var if XSTR (exp, 0) =~ /\b${var}\b/o; */
    case MATCH_TEST:
      {
	const char *p = XSTR (exp, 0);
	const char *q = strstr (p, var);
	if (!q)
	  return false;
	if (q != p && (ISALNUM (q[-1]) || q[-1] == '_'))
	  return false;
	q += strlen (var);
	if (ISALNUM (q[0]) || q[0] == '_')
	  return false;
      }
      return true;

    default:
      gcc_unreachable ();
    }
}

/* Given an RTL expression EXP, find all subexpressions which we may
   assume to perform mode tests.  Normal MATCH_OPERAND does;
   MATCH_CODE doesn't as such (although certain codes always have
   VOIDmode); and we have to assume that MATCH_TEST does not.
   These combine in almost-boolean fashion - the only exception is
   that (not X) must be assumed not to perform a mode test, whether
   or not X does.

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
      gcc_unreachable ();
    }
}

/* Determine whether the expression EXP is a MATCH_CODE that should
   be written as a switch statement.  */
static bool
generate_switch_p (rtx exp)
{
  return GET_CODE (exp) == MATCH_CODE
	 && strchr (XSTR (exp, 0), ',');
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

  /* Check whether the predicate accepts const scalar ints (which always
     have a stored mode of VOIDmode, but logically have a real mode)
     and whether it matches anything besides const scalar ints.  */
  bool matches_const_scalar_int_p = false;
  bool matches_other_p = false;
  for (int i = 0; i < NUM_RTX_CODE; ++i)
    if (p->codes[i])
      switch (i)
	{
	case CONST_INT:
	case CONST_WIDE_INT:
	  /* Special handling for (VOIDmode) LABEL_REFs.  */
	case LABEL_REF:
	  matches_const_scalar_int_p = true;
	  break;

	case CONST_DOUBLE:
	  if (!TARGET_SUPPORTS_WIDE_INT)
	    matches_const_scalar_int_p = true;
	  matches_other_p = true;
	  break;

	default:
	  matches_other_p = true;
	  break;
	}

  /* There's no need for a mode check if the predicate only accepts
     constant integers.  The code checks in the predicate are enough
     to establish that the mode is VOIDmode.

     Note that the predicate itself should check whether a scalar
     integer is in range of the given mode.  */
  if (!matches_other_p)
    return;

  mark_mode_tests (p->exp);

  /* If the whole expression already tests the mode, we're done.  */
  if (!NO_MODE_TEST (p->exp))
    return;

  match_test_exp = rtx_alloc (MATCH_TEST);
  if (matches_const_scalar_int_p)
    XSTR (match_test_exp, 0) = ("mode == VOIDmode || GET_MODE (op) == mode"
				" || GET_MODE (op) == VOIDmode");
  else
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
	case AND:
	  /* The switch code generation in write_predicate_stmts prefers
	     rtx code tests to be at the top of the expression tree.  So
	     push this AND down into the second operand of an existing
	     AND expression.  */
	  if (generate_switch_p (XEXP (subexp, 0)))
	    pos = &XEXP (subexp, 1);
	  goto break_loop;

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

/* PATH is a string describing a path from the root of an RTL
   expression to an inner subexpression to be tested.  Output
   code which computes the subexpression from the variable
   holding the root of the expression.  */
static void
write_extract_subexp (const char *path)
{
  int len = strlen (path);
  int i;

  /* We first write out the operations (XEXP or XVECEXP) in reverse
     order, then write "op", then the indices in forward order.  */
  for (i = len - 1; i >= 0; i--)
    {
      if (ISLOWER (path[i]))
	fputs ("XVECEXP (", stdout);
      else if (ISDIGIT (path[i]))
	fputs ("XEXP (", stdout);
      else
	gcc_unreachable ();
    }

  fputs ("op", stdout);

  for (i = 0; i < len; i++)
    {
      if (ISLOWER (path[i]))
	printf (", 0, %d)", path[i] - 'a');
      else if (ISDIGIT (path[i]))
	printf (", %d)", path[i] - '0');
      else
	gcc_unreachable ();
    }
}

/* CODES is a list of RTX codes.  Write out an expression which
   determines whether the operand has one of those codes.  */
static void
write_match_code (const char *path, const char *codes)
{
  const char *code;

  while ((code = scan_comma_elt (&codes)) != 0)
    {
      fputs ("GET_CODE (", stdout);
      write_extract_subexp (path);
      fputs (") == ", stdout);
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
write_predicate_expr (rtx exp)
{
  switch (GET_CODE (exp))
    {
    case AND:
      putchar ('(');
      write_predicate_expr (XEXP (exp, 0));
      fputs (") && (", stdout);
      write_predicate_expr (XEXP (exp, 1));
      putchar (')');
      break;

    case IOR:
      putchar ('(');
      write_predicate_expr (XEXP (exp, 0));
      fputs (") || (", stdout);
      write_predicate_expr (XEXP (exp, 1));
      putchar (')');
      break;

    case NOT:
      fputs ("!(", stdout);
      write_predicate_expr (XEXP (exp, 0));
      putchar (')');
      break;

    case IF_THEN_ELSE:
      putchar ('(');
      write_predicate_expr (XEXP (exp, 0));
      fputs (") ? (", stdout);
      write_predicate_expr (XEXP (exp, 1));
      fputs (") : (", stdout);
      write_predicate_expr (XEXP (exp, 2));
      putchar (')');
      break;

    case MATCH_OPERAND:
      if (GET_MODE (exp) == VOIDmode)
        printf ("%s (op, mode)", XSTR (exp, 1));
      else
        printf ("%s (op, %smode)", XSTR (exp, 1), mode_name[GET_MODE (exp)]);
      break;

    case MATCH_CODE:
      write_match_code (XSTR (exp, 1), XSTR (exp, 0));
      break;

    case MATCH_TEST:
      rtx_reader_ptr->print_c_condition (XSTR (exp, 0));
      break;

    default:
      gcc_unreachable ();
    }
}

/* Write the MATCH_CODE expression EXP as a switch statement.  */

static void
write_match_code_switch (rtx exp)
{
  const char *codes = XSTR (exp, 0);
  const char *path = XSTR (exp, 1);
  const char *code;

  fputs ("  switch (GET_CODE (", stdout);
  write_extract_subexp (path);
  fputs ("))\n    {\n", stdout);

  while ((code = scan_comma_elt (&codes)) != 0)
    {
      fputs ("    case ", stdout);
      while (code < codes)
	{
	  putchar (TOUPPER (*code));
	  code++;
	}
      fputs (":\n", stdout);
    }
}

/* Given a predicate expression EXP, write out a sequence of stmts
   to evaluate it.  This is similar to write_predicate_expr but can
   generate efficient switch statements.  */

static void
write_predicate_stmts (rtx exp)
{
  switch (GET_CODE (exp))
    {
    case MATCH_CODE:
      if (generate_switch_p (exp))
	{
	  write_match_code_switch (exp);
	  puts ("      return true;\n"
		"    default:\n"
		"      break;\n"
		"    }\n"
		"  return false;");
	  return;
	}
      break;

    case AND:
      if (generate_switch_p (XEXP (exp, 0)))
	{
	  write_match_code_switch (XEXP (exp, 0));
	  puts ("      break;\n"
		"    default:\n"
		"      return false;\n"
		"    }");
	  exp = XEXP (exp, 1);
	}
      break;

    case IOR:
      if (generate_switch_p (XEXP (exp, 0)))
	{
	  write_match_code_switch (XEXP (exp, 0));
	  puts ("      return true;\n"
		"    default:\n"
		"      break;\n"
		"    }");
	  exp = XEXP (exp, 1);
	}
      break;

    case NOT:
      if (generate_switch_p (XEXP (exp, 0)))
	{
	  write_match_code_switch (XEXP (exp, 0));
	  puts ("      return false;\n"
		"    default:\n"
		"      break;\n"
		"    }\n"
		"  return true;");
	  return;
	}
      break;

    default:
      break;
    }

  fputs ("  return ",stdout);
  write_predicate_expr (exp);
  fputs (";\n", stdout);
}

/* Given a predicate, write out a complete C function to compute it.  */
static void
write_one_predicate_function (struct pred_data *p)
{
  if (!p->exp)
    return;

  write_predicate_subfunction (p);
  add_mode_tests (p);

  /* A normal predicate can legitimately not look at machine_mode
     if it accepts only CONST_INTs and/or CONST_WIDE_INT and/or CONST_DOUBLEs.  */
  printf ("int\n%s (rtx op, machine_mode mode ATTRIBUTE_UNUSED)\n{\n",
	  p->name);
  write_predicate_stmts (p->exp);
  fputs ("}\n\n", stdout);
}

/* Constraints fall into two categories: register constraints
   (define_register_constraint), and others (define_constraint,
   define_memory_constraint, define_special_memory_constraint,
   define_address_constraint).  We work out automatically which of the
   various old-style macros they correspond to, and produce
   appropriate code.  They all go in the same hash table so we can
   verify that there are no duplicate names.  */

/* All data from one constraint definition.  */
struct constraint_data
{
  struct constraint_data *next_this_letter;
  struct constraint_data *next_textual;
  const char *name;
  const char *c_name;    /* same as .name unless mangling is necessary */
  file_location loc;     /* location of definition */
  size_t namelen;
  const char *regclass;  /* for register constraints */
  rtx exp;               /* for other constraints */
  unsigned int is_register	: 1;
  unsigned int is_const_int	: 1;
  unsigned int is_const_dbl	: 1;
  unsigned int is_extra		: 1;
  unsigned int is_memory	: 1;
  unsigned int is_special_memory: 1;
  unsigned int is_address	: 1;
  unsigned int maybe_allows_reg : 1;
  unsigned int maybe_allows_mem : 1;
};

/* Overview of all constraints beginning with a given letter.  */

static struct constraint_data *
constraints_by_letter_table[1<<CHAR_BIT];

/* For looking up all the constraints in the order that they appeared
   in the machine description.  */
static struct constraint_data *first_constraint;
static struct constraint_data **last_constraint_ptr = &first_constraint;

#define FOR_ALL_CONSTRAINTS(iter_) \
  for (iter_ = first_constraint; iter_; iter_ = iter_->next_textual)

/* Contraint letters that have a special meaning and that cannot be used
   in define*_constraints.  */
static const char generic_constraint_letters[] = "g";

/* Machine-independent code expects that constraints with these
   (initial) letters will allow only (a subset of all) CONST_INTs.  */

static const char const_int_constraints[] = "IJKLMNOP";

/* Machine-independent code expects that constraints with these
   (initial) letters will allow only (a subset of all) CONST_DOUBLEs.  */

static const char const_dbl_constraints[] = "GH";

/* Summary data used to decide whether to output various functions and
   macro definitions.  */
static unsigned int constraint_max_namelen;
static bool have_register_constraints;
static bool have_memory_constraints;
static bool have_special_memory_constraints;
static bool have_address_constraints;
static bool have_extra_constraints;
static bool have_const_int_constraints;
static unsigned int num_constraints;

static const constraint_data **enum_order;
static unsigned int register_start, register_end;
static unsigned int satisfied_start;
static unsigned int const_int_start, const_int_end;
static unsigned int memory_start, memory_end;
static unsigned int special_memory_start, special_memory_end;
static unsigned int address_start, address_end;
static unsigned int maybe_allows_none_start, maybe_allows_none_end;
static unsigned int maybe_allows_reg_start, maybe_allows_reg_end;
static unsigned int maybe_allows_mem_start, maybe_allows_mem_end;

/* Convert NAME, which contains angle brackets and/or underscores, to
   a string that can be used as part of a C identifier.  The string
   comes from the rtl_obstack.  */
static const char *
mangle (const char *name)
{
  for (; *name; name++)
    switch (*name)
      {
      case '_': obstack_grow (rtl_obstack, "__", 2); break;
      case '<':	obstack_grow (rtl_obstack, "_l", 2); break;
      case '>':	obstack_grow (rtl_obstack, "_g", 2); break;
      default: obstack_1grow (rtl_obstack, *name); break;
      }

  obstack_1grow (rtl_obstack, '\0');
  return XOBFINISH (rtl_obstack, const char *);
}

/* Add one constraint, of any sort, to the tables.  NAME is its name;
   REGCLASS is the register class, if any; EXP is the expression to
   test, if any; IS_MEMORY, IS_SPECIAL_MEMORY and IS_ADDRESS indicate
   memory, special memory, and address constraints, respectively; LOC
   is the .md file location.

   Not all combinations of arguments are valid; most importantly,
   REGCLASS is mutually exclusive with EXP, and
   IS_MEMORY/IS_SPECIAL_MEMORY/IS_ADDRESS are only meaningful for
   constraints with EXP.

   This function enforces all syntactic and semantic rules about what
   constraints can be defined.  */

static void
add_constraint (const char *name, const char *regclass,
		rtx exp, bool is_memory, bool is_special_memory,
		bool is_address, file_location loc)
{
  struct constraint_data *c, **iter, **slot;
  const char *p;
  bool need_mangled_name = false;
  bool is_const_int;
  bool is_const_dbl;
  size_t namelen;

  if (strcmp (name, "TARGET_MEM_CONSTRAINT") == 0)
    name = general_mem;

  if (exp && validate_exp (exp, name, loc))
    return;

  for (p = name; *p; p++)
    if (!ISALNUM (*p))
      {
	if (*p == '<' || *p == '>' || *p == '_')
	  need_mangled_name = true;
	else
	  {
	    error_at (loc, "constraint name '%s' must be composed of letters,"
		      " digits, underscores, and angle brackets", name);
	    return;
	  }
      }

  if (strchr (generic_constraint_letters, name[0]))
    {
      if (name[1] == '\0')
	error_at (loc, "constraint letter '%s' cannot be "
		  "redefined by the machine description", name);
      else
	error_at (loc, "constraint name '%s' cannot be defined by the machine"
		  " description, as it begins with '%c'", name, name[0]);
      return;
    }


  namelen = strlen (name);
  slot = &constraints_by_letter_table[(unsigned int)name[0]];
  for (iter = slot; *iter; iter = &(*iter)->next_this_letter)
    {
      /* This causes slot to end up pointing to the
	 next_this_letter field of the last constraint with a name
	 of equal or greater length than the new constraint; hence
	 the new constraint will be inserted after all previous
	 constraints with names of the same length.  */
      if ((*iter)->namelen >= namelen)
	slot = iter;

      if (!strcmp ((*iter)->name, name))
	{
	  error_at (loc, "redefinition of constraint '%s'", name);
	  message_at ((*iter)->loc, "previous definition is here");
	  return;
	}
      else if (!strncmp ((*iter)->name, name, (*iter)->namelen))
	{
	  error_at (loc, "defining constraint '%s' here", name);
	  message_at ((*iter)->loc, "renders constraint '%s' "
		      "(defined here) a prefix", (*iter)->name);
	  return;
	}
      else if (!strncmp ((*iter)->name, name, namelen))
	{
	  error_at (loc, "constraint '%s' is a prefix", name);
	  message_at ((*iter)->loc, "of constraint '%s' (defined here)",
		      (*iter)->name);
	  return;
	}
    }

  is_const_int = strchr (const_int_constraints, name[0]) != 0;
  is_const_dbl = strchr (const_dbl_constraints, name[0]) != 0;

  if (is_const_int || is_const_dbl)
    {
      enum rtx_code appropriate_code
	= is_const_int ? CONST_INT : CONST_DOUBLE;

      /* Consider relaxing this requirement in the future.  */
      if (regclass
	  || GET_CODE (exp) != AND
	  || GET_CODE (XEXP (exp, 0)) != MATCH_CODE
	  || strcmp (XSTR (XEXP (exp, 0), 0),
		     GET_RTX_NAME (appropriate_code)))
	{
	  if (name[1] == '\0')
	    error_at (loc, "constraint letter '%c' is reserved "
		      "for %s constraints", name[0],
		      GET_RTX_NAME (appropriate_code));
	  else
	    error_at (loc, "constraint names beginning with '%c' "
		      "(%s) are reserved for %s constraints",
		      name[0], name, GET_RTX_NAME (appropriate_code));
	  return;
	}

      if (is_memory)
	{
	  if (name[1] == '\0')
	    error_at (loc, "constraint letter '%c' cannot be a "
		      "memory constraint", name[0]);
	  else
	    error_at (loc, "constraint name '%s' begins with '%c', "
		      "and therefore cannot be a memory constraint",
		      name, name[0]);
	  return;
	}
      else if (is_special_memory)
	{
	  if (name[1] == '\0')
	    error_at (loc, "constraint letter '%c' cannot be a "
		      "special memory constraint", name[0]);
	  else
	    error_at (loc, "constraint name '%s' begins with '%c', "
		      "and therefore cannot be a special memory constraint",
		      name, name[0]);
	  return;
	}
      else if (is_address)
	{
	  if (name[1] == '\0')
	    error_at (loc, "constraint letter '%c' cannot be an "
		      "address constraint", name[0]);
	  else
	    error_at (loc, "constraint name '%s' begins with '%c', "
		      "and therefore cannot be an address constraint",
		      name, name[0]);
	  return;
	}
    }


  c = XOBNEW (rtl_obstack, struct constraint_data);
  c->name = name;
  c->c_name = need_mangled_name ? mangle (name) : name;
  c->loc = loc;
  c->namelen = namelen;
  c->regclass = regclass;
  c->exp = exp;
  c->is_register = regclass != 0;
  c->is_const_int = is_const_int;
  c->is_const_dbl = is_const_dbl;
  c->is_extra = !(regclass || is_const_int || is_const_dbl);
  c->is_memory = is_memory;
  c->is_special_memory = is_special_memory;
  c->is_address = is_address;
  c->maybe_allows_reg = true;
  c->maybe_allows_mem = true;
  if (exp)
    {
      char codes[NUM_RTX_CODE];
      compute_test_codes (exp, loc, codes);
      if (!codes[REG] && !codes[SUBREG])
	c->maybe_allows_reg = false;
      if (!codes[MEM])
	c->maybe_allows_mem = false;
    }
  c->next_this_letter = *slot;
  *slot = c;

  /* Insert this constraint in the list of all constraints in textual
     order.  */
  c->next_textual = 0;
  *last_constraint_ptr = c;
  last_constraint_ptr = &c->next_textual;

  constraint_max_namelen = MAX (constraint_max_namelen, strlen (name));
  have_register_constraints |= c->is_register;
  have_const_int_constraints |= c->is_const_int;
  have_extra_constraints |= c->is_extra;
  have_memory_constraints |= c->is_memory;
  have_special_memory_constraints |= c->is_special_memory;
  have_address_constraints |= c->is_address;
  num_constraints += 1;
}

/* Process a DEFINE_CONSTRAINT, DEFINE_MEMORY_CONSTRAINT,
   DEFINE_SPECIAL_MEMORY_CONSTRAINT, or DEFINE_ADDRESS_CONSTRAINT
   expression, C.  */
static void
process_define_constraint (md_rtx_info *info)
{
  add_constraint (XSTR (info->def, 0), 0, XEXP (info->def, 2),
		  GET_CODE (info->def) == DEFINE_MEMORY_CONSTRAINT,
		  GET_CODE (info->def) == DEFINE_SPECIAL_MEMORY_CONSTRAINT,
		  GET_CODE (info->def) == DEFINE_ADDRESS_CONSTRAINT,
		  info->loc);
}

/* Process a DEFINE_REGISTER_CONSTRAINT expression, C.  */
static void
process_define_register_constraint (md_rtx_info *info)
{
  add_constraint (XSTR (info->def, 0), XSTR (info->def, 1),
		  0, false, false, false, info->loc);
}

/* Put the constraints into enum order.  We want to keep constraints
   of the same type together so that query functions can be simple
   range checks.  */
static void
choose_enum_order (void)
{
  struct constraint_data *c;

  enum_order = XNEWVEC (const constraint_data *, num_constraints);
  unsigned int next = 0;

  register_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (c->is_register)
      enum_order[next++] = c;
  register_end = next;

  satisfied_start = next;

  const_int_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (c->is_const_int)
      enum_order[next++] = c;
  const_int_end = next;

  memory_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (c->is_memory)
      enum_order[next++] = c;
  memory_end = next;

  special_memory_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (c->is_special_memory)
      enum_order[next++] = c;
  special_memory_end = next;

  address_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (c->is_address)
      enum_order[next++] = c;
  address_end = next;

  maybe_allows_none_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (!c->is_register && !c->is_const_int && !c->is_memory
	&& !c->is_special_memory && !c->is_address
	&& !c->maybe_allows_reg && !c->maybe_allows_mem)
      enum_order[next++] = c;
  maybe_allows_none_end = next;

  maybe_allows_reg_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (!c->is_register && !c->is_const_int && !c->is_memory
	&& !c->is_special_memory && !c->is_address
	&& c->maybe_allows_reg && !c->maybe_allows_mem)
      enum_order[next++] = c;
  maybe_allows_reg_end = next;

  maybe_allows_mem_start = next;
  FOR_ALL_CONSTRAINTS (c)
    if (!c->is_register && !c->is_const_int && !c->is_memory
	&& !c->is_special_memory && !c->is_address
	&& !c->maybe_allows_reg && c->maybe_allows_mem)
      enum_order[next++] = c;
  maybe_allows_mem_end = next;

  FOR_ALL_CONSTRAINTS (c)
    if (!c->is_register && !c->is_const_int && !c->is_memory
	&& !c->is_special_memory && !c->is_address
	&& c->maybe_allows_reg && c->maybe_allows_mem)
      enum_order[next++] = c;
  gcc_assert (next == num_constraints);
}

/* Write out an enumeration with one entry per machine-specific
   constraint.  */
static void
write_enum_constraint_num (void)
{
  fputs ("#define CONSTRAINT_NUM_DEFINED_P 1\n", stdout);
  fputs ("enum constraint_num\n"
	 "{\n"
	 "  CONSTRAINT__UNKNOWN = 0", stdout);
  for (unsigned int i = 0; i < num_constraints; ++i)
    printf (",\n  CONSTRAINT_%s", enum_order[i]->c_name);
  puts (",\n  CONSTRAINT__LIMIT\n};\n");
}

/* Write out a function which looks at a string and determines what
   constraint name, if any, it begins with.  */
static void
write_lookup_constraint_1 (void)
{
  unsigned int i;
  puts ("enum constraint_num\n"
	"lookup_constraint_1 (const char *str)\n"
	"{\n"
	"  switch (str[0])\n"
	"    {");

  for (i = 0; i < ARRAY_SIZE (constraints_by_letter_table); i++)
    {
      struct constraint_data *c = constraints_by_letter_table[i];
      if (!c)
	continue;

      printf ("    case '%c':\n", i);
      if (c->namelen == 1)
	printf ("      return CONSTRAINT_%s;\n", c->c_name);
      else
	{
	  do
	    {
	      printf ("      if (!strncmp (str + 1, \"%s\", %lu))\n"
		      "        return CONSTRAINT_%s;\n",
		      c->name + 1, (unsigned long int) c->namelen - 1,
		      c->c_name);
	      c = c->next_this_letter;
	    }
	  while (c);
	  puts ("      break;");
	}
    }

  puts ("    default: break;\n"
	"    }\n"
	"  return CONSTRAINT__UNKNOWN;\n"
	"}\n");
}

/* Write out an array that maps single-letter characters to their
   constraints (if that fits in a character) or 255 if lookup_constraint_1
   must be called.  */
static void
write_lookup_constraint_array (void)
{
  unsigned int i;
  printf ("const unsigned char lookup_constraint_array[] = {\n  ");
  for (i = 0; i < ARRAY_SIZE (constraints_by_letter_table); i++)
    {
      if (i != 0)
	printf (",\n  ");
      struct constraint_data *c = constraints_by_letter_table[i];
      if (!c)
	printf ("CONSTRAINT__UNKNOWN");
      else if (c->namelen == 1)
	printf ("MIN ((int) CONSTRAINT_%s, (int) UCHAR_MAX)", c->c_name);
      else
	printf ("UCHAR_MAX");
    }
  printf ("\n};\n\n");
}

/* Write out a function which looks at a string and determines what
   the constraint name length is.  */
static void
write_insn_constraint_len (void)
{
  unsigned int i;

  puts ("static inline size_t\n"
	"insn_constraint_len (char fc, const char *str ATTRIBUTE_UNUSED)\n"
	"{\n"
	"  switch (fc)\n"
	"    {");

  for (i = 0; i < ARRAY_SIZE (constraints_by_letter_table); i++)
    {
      struct constraint_data *c = constraints_by_letter_table[i];

      if (!c
      	  || c->namelen == 1)
	continue;

      /* Constraints with multiple characters should have the same
	 length.  */
      {
	struct constraint_data *c2 = c->next_this_letter;
	size_t len = c->namelen;
	while (c2)
	  {
	    if (c2->namelen != len)
	      error ("Multi-letter constraints with first letter '%c' "
		     "should have same length", i);
	    c2 = c2->next_this_letter;
	  }
      }

      printf ("    case '%c': return %lu;\n",
	      i, (unsigned long int) c->namelen);
    }

  puts ("    default: break;\n"
	"    }\n"
	"  return 1;\n"
	"}\n");
}

/* Write out the function which computes the register class corresponding
   to a register constraint.  */
static void
write_reg_class_for_constraint_1 (void)
{
  struct constraint_data *c;

  puts ("enum reg_class\n"
	"reg_class_for_constraint_1 (enum constraint_num c)\n"
	"{\n"
	"  switch (c)\n"
	"    {");

  FOR_ALL_CONSTRAINTS (c)
    if (c->is_register)
      printf ("    case CONSTRAINT_%s: return %s;\n", c->c_name, c->regclass);

  puts ("    default: break;\n"
	"    }\n"
	"  return NO_REGS;\n"
	"}\n");
}

/* Write out the functions which compute whether a given value matches
   a given non-register constraint.  */
static void
write_tm_constrs_h (void)
{
  struct constraint_data *c;

  printf ("\
/* Generated automatically by the program '%s'\n\
   from the machine description file '%s'.  */\n\n", progname,
	  md_reader_ptr->get_top_level_filename ());

  puts ("\
#ifndef GCC_TM_CONSTRS_H\n\
#define GCC_TM_CONSTRS_H\n");

  FOR_ALL_CONSTRAINTS (c)
    if (!c->is_register)
      {
	bool needs_ival = needs_variable (c->exp, "ival");
	bool needs_hval = needs_variable (c->exp, "hval");
	bool needs_lval = needs_variable (c->exp, "lval");
	bool needs_rval = needs_variable (c->exp, "rval");
	bool needs_mode = (needs_variable (c->exp, "mode")
			   || needs_hval || needs_lval || needs_rval);
	bool needs_op = (needs_variable (c->exp, "op")
			 || needs_ival || needs_mode);

	printf ("static inline bool\n"
		"satisfies_constraint_%s (rtx %s)\n"
		"{\n", c->c_name,
		needs_op ? "op" : "ARG_UNUSED (op)");
	if (needs_mode)
	  puts ("  machine_mode mode = GET_MODE (op);");
	if (needs_ival)
	  puts ("  HOST_WIDE_INT ival = 0;");
	if (needs_hval)
	  puts ("  HOST_WIDE_INT hval = 0;");
	if (needs_lval)
	  puts ("  unsigned HOST_WIDE_INT lval = 0;");
	if (needs_rval)
	  puts ("  const REAL_VALUE_TYPE *rval = 0;");

	if (needs_ival)
	  puts ("  if (CONST_INT_P (op))\n"
		"    ival = INTVAL (op);");
#if TARGET_SUPPORTS_WIDE_INT
	if (needs_lval || needs_hval)
	  error ("you can't use lval or hval");
#else
	if (needs_hval)
	  puts ("  if (GET_CODE (op) == CONST_DOUBLE && mode == VOIDmode)"
		"    hval = CONST_DOUBLE_HIGH (op);");
	if (needs_lval)
	  puts ("  if (GET_CODE (op) == CONST_DOUBLE && mode == VOIDmode)"
		"    lval = CONST_DOUBLE_LOW (op);");
#endif
	if (needs_rval)
	  puts ("  if (GET_CODE (op) == CONST_DOUBLE && mode != VOIDmode)"
		"    rval = CONST_DOUBLE_REAL_VALUE (op);");

	write_predicate_stmts (c->exp);
	fputs ("}\n", stdout);
      }
  puts ("#endif /* tm-constrs.h */");
}

/* Write out the wrapper function, constraint_satisfied_p, that maps
   a CONSTRAINT_xxx constant to one of the predicate functions generated
   above.  */
static void
write_constraint_satisfied_p_array (void)
{
  if (satisfied_start == num_constraints)
    return;

  printf ("bool (*constraint_satisfied_p_array[]) (rtx) = {\n  ");
  for (unsigned int i = satisfied_start; i < num_constraints; ++i)
    {
      if (i != satisfied_start)
	printf (",\n  ");
      printf ("satisfies_constraint_%s", enum_order[i]->c_name);
    }
  printf ("\n};\n\n");
}

/* Write out the function which computes whether a given value matches
   a given CONST_INT constraint.  This doesn't just forward to
   constraint_satisfied_p because caller passes the INTVAL, not the RTX.  */
static void
write_insn_const_int_ok_for_constraint (void)
{
  struct constraint_data *c;

  puts ("bool\n"
	"insn_const_int_ok_for_constraint (HOST_WIDE_INT ival, "
	                                  "enum constraint_num c)\n"
	"{\n"
	"  switch (c)\n"
	"    {");

  FOR_ALL_CONSTRAINTS (c)
    if (c->is_const_int)
      {
	printf ("    case CONSTRAINT_%s:\n      return ", c->c_name);
	/* c->exp is guaranteed to be (and (match_code "const_int") (...));
	   we know at this point that we have a const_int, so we need not
	   bother with that part of the test.  */
	write_predicate_expr (XEXP (c->exp, 1));
	fputs (";\n\n", stdout);
      }

  puts ("    default: break;\n"
	"    }\n"
	"  return false;\n"
	"}\n");
}

/* Write a definition for a function NAME that returns true if a given
   constraint_num is in the range [START, END).  */
static void
write_range_function (const char *name, unsigned int start, unsigned int end)
{
  printf ("static inline bool\n");
  if (start != end)
    printf ("%s (enum constraint_num c)\n"
	    "{\n"
	    "  return c >= CONSTRAINT_%s && c <= CONSTRAINT_%s;\n"
	    "}\n\n",
	    name, enum_order[start]->c_name, enum_order[end - 1]->c_name);
  else
    printf ("%s (enum constraint_num)\n"
	    "{\n"
	    "  return false;\n"
	    "}\n\n", name);
}

/* Write a definition for insn_extra_constraint_allows_reg_mem function.  */
static void
write_allows_reg_mem_function (void)
{
  printf ("static inline void\n"
	  "insn_extra_constraint_allows_reg_mem (enum constraint_num c,\n"
	  "\t\t\t\t      bool *allows_reg, bool *allows_mem)\n"
	  "{\n");
  if (maybe_allows_none_start != maybe_allows_none_end)
    printf ("  if (c >= CONSTRAINT_%s && c <= CONSTRAINT_%s)\n"
	    "    return;\n",
	    enum_order[maybe_allows_none_start]->c_name,
	    enum_order[maybe_allows_none_end - 1]->c_name);
  if (maybe_allows_reg_start != maybe_allows_reg_end)
    printf ("  if (c >= CONSTRAINT_%s && c <= CONSTRAINT_%s)\n"
	    "    {\n"
	    "      *allows_reg = true;\n"
	    "      return;\n"
	    "    }\n",
	    enum_order[maybe_allows_reg_start]->c_name,
	    enum_order[maybe_allows_reg_end - 1]->c_name);
  if (maybe_allows_mem_start != maybe_allows_mem_end)
    printf ("  if (c >= CONSTRAINT_%s && c <= CONSTRAINT_%s)\n"
	    "    {\n"
	    "      *allows_mem = true;\n"
	    "      return;\n"
	    "    }\n",
	    enum_order[maybe_allows_mem_start]->c_name,
	    enum_order[maybe_allows_mem_end - 1]->c_name);
  printf ("  (void) c;\n"
	  "  *allows_reg = true;\n"
	  "  *allows_mem = true;\n"
	  "}\n\n");
}

/* VEC is a list of key/value pairs, with the keys being lower bounds
   of a range.  Output a decision tree that handles the keys covered by
   [VEC[START], VEC[END]), returning FALLBACK for keys lower then VEC[START]'s.
   INDENT is the number of spaces to indent the code.  */
static void
print_type_tree (const vec <std::pair <unsigned int, const char *> > &vec,
		 unsigned int start, unsigned int end, const char *fallback,
		 unsigned int indent)
{
  while (start < end)
    {
      unsigned int mid = (start + end) / 2;
      printf ("%*sif (c >= CONSTRAINT_%s)\n",
	      indent, "", enum_order[vec[mid].first]->c_name);
      if (mid + 1 == end)
	print_type_tree (vec, mid + 1, end, vec[mid].second, indent + 2);
      else
	{
	  printf ("%*s{\n", indent + 2, "");
	  print_type_tree (vec, mid + 1, end, vec[mid].second, indent + 4);
	  printf ("%*s}\n", indent + 2, "");
	}
      end = mid;
    }
  printf ("%*sreturn %s;\n", indent, "", fallback);
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
   from the machine description file '%s'.  */\n\n", progname,
	  md_reader_ptr->get_top_level_filename ());

  puts ("\
#ifndef GCC_TM_PREDS_H\n\
#define GCC_TM_PREDS_H\n\
\n\
#ifdef HAVE_MACHINE_MODES");

  FOR_ALL_PREDICATES (p)
    printf ("extern int %s (rtx, machine_mode);\n", p->name);

  puts ("#endif /* HAVE_MACHINE_MODES */\n");

  if (constraint_max_namelen > 0)
    {
      write_enum_constraint_num ();
      puts ("extern enum constraint_num lookup_constraint_1 (const char *);\n"
	    "extern const unsigned char lookup_constraint_array[];\n"
	    "\n"
	    "/* Return the constraint at the beginning of P, or"
	    " CONSTRAINT__UNKNOWN if it\n"
	    "   isn't recognized.  */\n"
	    "\n"
	    "static inline enum constraint_num\n"
	    "lookup_constraint (const char *p)\n"
	    "{\n"
	    "  unsigned int index = lookup_constraint_array"
	    "[(unsigned char) *p];\n"
	    "  return (index == UCHAR_MAX\n"
	    "          ? lookup_constraint_1 (p)\n"
	    "          : (enum constraint_num) index);\n"
	    "}\n");
      if (satisfied_start == num_constraints)
	puts ("/* Return true if X satisfies constraint C.  */\n"
	      "\n"
	      "static inline bool\n"
	      "constraint_satisfied_p (rtx, enum constraint_num)\n"
	      "{\n"
	      "  return false;\n"
	      "}\n");
      else
	printf ("extern bool (*constraint_satisfied_p_array[]) (rtx);\n"
		"\n"
		"/* Return true if X satisfies constraint C.  */\n"
		"\n"
		"static inline bool\n"
		"constraint_satisfied_p (rtx x, enum constraint_num c)\n"
		"{\n"
		"  int i = (int) c - (int) CONSTRAINT_%s;\n"
		"  return i >= 0 && constraint_satisfied_p_array[i] (x);\n"
		"}\n"
		"\n",
		enum_order[satisfied_start]->name);

      write_range_function ("insn_extra_register_constraint",
			    register_start, register_end);
      write_range_function ("insn_extra_memory_constraint",
			    memory_start, memory_end);
      write_range_function ("insn_extra_special_memory_constraint",
			    special_memory_start, special_memory_end);
      write_range_function ("insn_extra_address_constraint",
			    address_start, address_end);
      write_allows_reg_mem_function ();

      if (constraint_max_namelen > 1)
        {
	  write_insn_constraint_len ();
	  puts ("#define CONSTRAINT_LEN(c_,s_) "
		"insn_constraint_len (c_,s_)\n");
	}
      else
	puts ("#define CONSTRAINT_LEN(c_,s_) 1\n");
      if (have_register_constraints)
	puts ("extern enum reg_class reg_class_for_constraint_1 "
	      "(enum constraint_num);\n"
	      "\n"
	      "static inline enum reg_class\n"
	      "reg_class_for_constraint (enum constraint_num c)\n"
	      "{\n"
	      "  if (insn_extra_register_constraint (c))\n"
	      "    return reg_class_for_constraint_1 (c);\n"
	      "  return NO_REGS;\n"
	      "}\n");
      else
	puts ("static inline enum reg_class\n"
	      "reg_class_for_constraint (enum constraint_num)\n"
	      "{\n"
	      "  return NO_REGS;\n"
	      "}\n");
      if (have_const_int_constraints)
	puts ("extern bool insn_const_int_ok_for_constraint "
	      "(HOST_WIDE_INT, enum constraint_num);\n"
	      "#define CONST_OK_FOR_CONSTRAINT_P(v_,c_,s_) \\\n"
	      "    insn_const_int_ok_for_constraint (v_, "
	      "lookup_constraint (s_))\n");
      else
	puts ("static inline bool\n"
	      "insn_const_int_ok_for_constraint (HOST_WIDE_INT,"
	      " enum constraint_num)\n"
	      "{\n"
	      "  return false;\n"
	      "}\n");

      puts ("enum constraint_type\n"
	    "{\n"
	    "  CT_REGISTER,\n"
	    "  CT_CONST_INT,\n"
	    "  CT_MEMORY,\n"
	    "  CT_SPECIAL_MEMORY,\n"
	    "  CT_ADDRESS,\n"
	    "  CT_FIXED_FORM\n"
	    "};\n"
	    "\n"
	    "static inline enum constraint_type\n"
	    "get_constraint_type (enum constraint_num c)\n"
	    "{");
      auto_vec <std::pair <unsigned int, const char *>, 4> values;
      if (const_int_start != const_int_end)
	values.safe_push (std::make_pair (const_int_start, "CT_CONST_INT"));
      if (memory_start != memory_end)
	values.safe_push (std::make_pair (memory_start, "CT_MEMORY"));
      if (special_memory_start != special_memory_end)
	values.safe_push (std::make_pair (special_memory_start, "CT_SPECIAL_MEMORY"));
      if (address_start != address_end)
	values.safe_push (std::make_pair (address_start, "CT_ADDRESS"));
      if (address_end != num_constraints)
	values.safe_push (std::make_pair (address_end, "CT_FIXED_FORM"));
      print_type_tree (values, 0, values.length (), "CT_REGISTER", 2);
      puts ("}");
    }

  puts ("#endif /* tm-preds.h */");
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
   from the machine description file '%s'.  */\n\n", progname,
	  md_reader_ptr->get_top_level_filename ());

  puts ("\
#define IN_TARGET_CODE 1\n\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"\n\
#include \"backend.h\"\n\
#include \"predict.h\"\n\
#include \"tree.h\"\n\
#include \"rtl.h\"\n\
#include \"alias.h\"\n\
#include \"varasm.h\"\n\
#include \"stor-layout.h\"\n\
#include \"calls.h\"\n\
#include \"memmodel.h\"\n\
#include \"tm_p.h\"\n\
#include \"insn-config.h\"\n\
#include \"recog.h\"\n\
#include \"output.h\"\n\
#include \"flags.h\"\n\
#include \"df.h\"\n\
#include \"resource.h\"\n\
#include \"diagnostic-core.h\"\n\
#include \"reload.h\"\n\
#include \"regs.h\"\n\
#include \"emit-rtl.h\"\n\
#include \"tm-constrs.h\"\n\
#include \"target.h\"\n");

  FOR_ALL_PREDICATES (p)
    write_one_predicate_function (p);

  if (constraint_max_namelen > 0)
    {
      write_lookup_constraint_1 ();
      write_lookup_constraint_array ();
      if (have_register_constraints)
	write_reg_class_for_constraint_1 ();
      write_constraint_satisfied_p_array ();

      if (have_const_int_constraints)
	write_insn_const_int_ok_for_constraint ();
    }
}

/* Argument parsing.  */
static bool gen_header;
static bool gen_constrs;

static bool
parse_option (const char *opt)
{
  if (!strcmp (opt, "-h"))
    {
      gen_header = true;
      return 1;
    }
  else if (!strcmp (opt, "-c"))
    {
      gen_constrs = true;
      return 1;
    }
  else
    return 0;
}

/* Master control.  */
int
main (int argc, const char **argv)
{
  progname = argv[0];
  if (argc <= 1)
    fatal ("no input file name");
  if (!init_rtx_reader_args_cb (argc, argv, parse_option))
    return FATAL_EXIT_CODE;

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_PREDICATE:
      case DEFINE_SPECIAL_PREDICATE:
	process_define_predicate (&info);
	break;

      case DEFINE_CONSTRAINT:
      case DEFINE_MEMORY_CONSTRAINT:
      case DEFINE_SPECIAL_MEMORY_CONSTRAINT:
      case DEFINE_ADDRESS_CONSTRAINT:
	process_define_constraint (&info);
	break;

      case DEFINE_REGISTER_CONSTRAINT:
	process_define_register_constraint (&info);
	break;

      default:
	break;
      }

  choose_enum_order ();

  if (gen_header)
    write_tm_preds_h ();
  else if (gen_constrs)
    write_tm_constrs_h ();
  else
    write_insn_preds_c ();

  if (have_error || ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
