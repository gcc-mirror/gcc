/* Matching subroutines in all sizes, shapes and colors.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "match.h"
#include "parse.h"

/* For matching and debugging purposes.  Order matters here!  The
   unary operators /must/ precede the binary plus and minus, or
   the expression parser breaks.  */

mstring intrinsic_operators[] = {
    minit ("+", INTRINSIC_UPLUS),
    minit ("-", INTRINSIC_UMINUS),
    minit ("+", INTRINSIC_PLUS),
    minit ("-", INTRINSIC_MINUS),
    minit ("**", INTRINSIC_POWER),
    minit ("//", INTRINSIC_CONCAT),
    minit ("*", INTRINSIC_TIMES),
    minit ("/", INTRINSIC_DIVIDE),
    minit (".and.", INTRINSIC_AND),
    minit (".or.", INTRINSIC_OR),
    minit (".eqv.", INTRINSIC_EQV),
    minit (".neqv.", INTRINSIC_NEQV),
    minit (".eq.", INTRINSIC_EQ),
    minit ("==", INTRINSIC_EQ),
    minit (".ne.", INTRINSIC_NE),
    minit ("/=", INTRINSIC_NE),
    minit (".ge.", INTRINSIC_GE),
    minit (">=", INTRINSIC_GE),
    minit (".le.", INTRINSIC_LE),
    minit ("<=", INTRINSIC_LE),
    minit (".lt.", INTRINSIC_LT),
    minit ("<", INTRINSIC_LT),
    minit (".gt.", INTRINSIC_GT),
    minit (">", INTRINSIC_GT),
    minit (".not.", INTRINSIC_NOT),
    minit (NULL, INTRINSIC_NONE)
};


/******************** Generic matching subroutines ************************/

/* In free form, match at least one space.  Always matches in fixed
   form.  */

match
gfc_match_space (void)
{
  locus old_loc;
  int c;

  if (gfc_current_form == FORM_FIXED)
    return MATCH_YES;

  old_loc = gfc_current_locus;

  c = gfc_next_char ();
  if (!gfc_is_whitespace (c))
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  gfc_gobble_whitespace ();

  return MATCH_YES;
}


/* Match an end of statement.  End of statement is optional
   whitespace, followed by a ';' or '\n' or comment '!'.  If a
   semicolon is found, we continue to eat whitespace and semicolons.  */

match
gfc_match_eos (void)
{
  locus old_loc;
  int flag, c;

  flag = 0;

  for (;;)
    {
      old_loc = gfc_current_locus;
      gfc_gobble_whitespace ();

      c = gfc_next_char ();
      switch (c)
	{
	case '!':
	  do
	    {
	      c = gfc_next_char ();
	    }
	  while (c != '\n');

	  /* Fall through */

	case '\n':
	  return MATCH_YES;

	case ';':
	  flag = 1;
	  continue;
	}

      break;
    }

  gfc_current_locus = old_loc;
  return (flag) ? MATCH_YES : MATCH_NO;
}


/* Match a literal integer on the input, setting the value on
   MATCH_YES.  Literal ints occur in kind-parameters as well as
   old-style character length specifications.  */

match
gfc_match_small_literal_int (int *value)
{
  locus old_loc;
  char c;
  int i;

  old_loc = gfc_current_locus;

  gfc_gobble_whitespace ();
  c = gfc_next_char ();

  if (!ISDIGIT (c))
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  i = c - '0';

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char ();

      if (!ISDIGIT (c))
	break;

      i = 10 * i + c - '0';

      if (i > 99999999)
	{
	  gfc_error ("Integer too large at %C");
	  return MATCH_ERROR;
	}
    }

  gfc_current_locus = old_loc;

  *value = i;
  return MATCH_YES;
}


/* Match a small, constant integer expression, like in a kind
   statement.  On MATCH_YES, 'value' is set.  */

match
gfc_match_small_int (int *value)
{
  gfc_expr *expr;
  const char *p;
  match m;
  int i;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    return m;

  p = gfc_extract_int (expr, &i);
  gfc_free_expr (expr);

  if (p != NULL)
    {
      gfc_error (p);
      m = MATCH_ERROR;
    }

  *value = i;
  return m;
}


/* Matches a statement label.  Uses gfc_match_small_literal_int() to
   do most of the work.  */

match
gfc_match_st_label (gfc_st_label ** label, int allow_zero)
{
  locus old_loc;
  match m;
  int i;

  old_loc = gfc_current_locus;

  m = gfc_match_small_literal_int (&i);
  if (m != MATCH_YES)
    return m;

  if (((i == 0) && allow_zero) || i <= 99999)
    {
      *label = gfc_get_st_label (i);
      return MATCH_YES;
    }

  gfc_error ("Statement label at %C is out of range");
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}


/* Match and validate a label associated with a named IF, DO or SELECT
   statement.  If the symbol does not have the label attribute, we add
   it.  We also make sure the symbol does not refer to another
   (active) block.  A matched label is pointed to by gfc_new_block.  */

match
gfc_match_label (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_state_data *p;
  match m;

  gfc_new_block = NULL;

  m = gfc_match (" %n :", name);
  if (m != MATCH_YES)
    return m;

  if (gfc_get_symbol (name, NULL, &gfc_new_block))
    {
      gfc_error ("Label name '%s' at %C is ambiguous", name);
      return MATCH_ERROR;
    }

  if (gfc_new_block->attr.flavor != FL_LABEL
      && gfc_add_flavor (&gfc_new_block->attr, FL_LABEL,
			 gfc_new_block->name, NULL) == FAILURE)
    return MATCH_ERROR;

  for (p = gfc_state_stack; p; p = p->previous)
    if (p->sym == gfc_new_block)
      {
	gfc_error ("Label %s at %C already in use by a parent block",
		   gfc_new_block->name);
	return MATCH_ERROR;
      }

  return MATCH_YES;
}


/* Try and match the input against an array of possibilities.  If one
   potential matching string is a substring of another, the longest
   match takes precedence.  Spaces in the target strings are optional
   spaces that do not necessarily have to be found in the input
   stream.  In fixed mode, spaces never appear.  If whitespace is
   matched, it matches unlimited whitespace in the input.  For this
   reason, the 'mp' member of the mstring structure is used to track
   the progress of each potential match.

   If there is no match we return the tag associated with the
   terminating NULL mstring structure and leave the locus pointer
   where it started.  If there is a match we return the tag member of
   the matched mstring and leave the locus pointer after the matched
   character.

   A '%' character is a mandatory space.  */

int
gfc_match_strings (mstring * a)
{
  mstring *p, *best_match;
  int no_match, c, possibles;
  locus match_loc;

  possibles = 0;

  for (p = a; p->string != NULL; p++)
    {
      p->mp = p->string;
      possibles++;
    }

  no_match = p->tag;

  best_match = NULL;
  match_loc = gfc_current_locus;

  gfc_gobble_whitespace ();

  while (possibles > 0)
    {
      c = gfc_next_char ();

      /* Apply the next character to the current possibilities.  */
      for (p = a; p->string != NULL; p++)
	{
	  if (p->mp == NULL)
	    continue;

	  if (*p->mp == ' ')
	    {
	      /* Space matches 1+ whitespace(s).  */
	      if ((gfc_current_form == FORM_FREE)
		  && gfc_is_whitespace (c))
		continue;

	      p->mp++;
	    }

	  if (*p->mp != c)
	    {
	      /* Match failed.  */
	      p->mp = NULL;
	      possibles--;
	      continue;
	    }

	  p->mp++;
	  if (*p->mp == '\0')
	    {
	      /* Found a match.  */
	      match_loc = gfc_current_locus;
	      best_match = p;
	      possibles--;
	      p->mp = NULL;
	    }
	}
    }

  gfc_current_locus = match_loc;

  return (best_match == NULL) ? no_match : best_match->tag;
}


/* See if the current input looks like a name of some sort.  Modifies
   the passed buffer which must be GFC_MAX_SYMBOL_LEN+1 bytes long.  */

match
gfc_match_name (char *buffer)
{
  locus old_loc;
  int i, c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  c = gfc_next_char ();
  if (!ISALPHA (c))
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  i = 0;

  do
    {
      buffer[i++] = c;

      if (i > gfc_option.max_identifier_length)
	{
	  gfc_error ("Name at %C is too long");
	  return MATCH_ERROR;
	}

      old_loc = gfc_current_locus;
      c = gfc_next_char ();
    }
  while (ISALNUM (c)
	 || c == '_'
	 || (gfc_option.flag_dollar_ok && c == '$'));

  buffer[i] = '\0';
  gfc_current_locus = old_loc;

  return MATCH_YES;
}


/* Match a symbol on the input.  Modifies the pointer to the symbol
   pointer if successful.  */

match
gfc_match_sym_tree (gfc_symtree ** matched_symbol, int host_assoc)
{
  char buffer[GFC_MAX_SYMBOL_LEN + 1];
  match m;

  m = gfc_match_name (buffer);
  if (m != MATCH_YES)
    return m;

  if (host_assoc)
    return (gfc_get_ha_sym_tree (buffer, matched_symbol))
      ? MATCH_ERROR : MATCH_YES;

  if (gfc_get_sym_tree (buffer, NULL, matched_symbol))
    return MATCH_ERROR;

  return MATCH_YES;
}


match
gfc_match_symbol (gfc_symbol ** matched_symbol, int host_assoc)
{
  gfc_symtree *st;
  match m;

  m = gfc_match_sym_tree (&st, host_assoc);

  if (m == MATCH_YES)
    {
      if (st)
        *matched_symbol = st->n.sym;
      else
        *matched_symbol = NULL;
    }
  return m;
}

/* Match an intrinsic operator.  Returns an INTRINSIC enum. While matching, 
   we always find INTRINSIC_PLUS before INTRINSIC_UPLUS. We work around this 
   in matchexp.c.  */

match
gfc_match_intrinsic_op (gfc_intrinsic_op * result)
{
  gfc_intrinsic_op op;

  op = (gfc_intrinsic_op) gfc_match_strings (intrinsic_operators);

  if (op == INTRINSIC_NONE)
    return MATCH_NO;

  *result = op;
  return MATCH_YES;
}


/* Match a loop control phrase:

    <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]

   If the final integer expression is not present, a constant unity
   expression is returned.  We don't return MATCH_ERROR until after
   the equals sign is seen.  */

match
gfc_match_iterator (gfc_iterator * iter, int init_flag)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *var, *e1, *e2, *e3;
  locus start;
  match m;

  /* Match the start of an iterator without affecting the symbol
     table.  */

  start = gfc_current_locus;
  m = gfc_match (" %n =", name);
  gfc_current_locus = start;

  if (m != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_variable (&var, 0);
  if (m != MATCH_YES)
    return MATCH_NO;

  gfc_match_char ('=');

  e1 = e2 = e3 = NULL;

  if (var->ref != NULL)
    {
      gfc_error ("Loop variable at %C cannot be a sub-component");
      goto cleanup;
    }

  if (var->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Loop variable '%s' at %C cannot be INTENT(IN)",
		 var->symtree->n.sym->name);
      goto cleanup;
    }

  if (var->symtree->n.sym->attr.pointer)
    {
      gfc_error ("Loop variable at %C cannot have the POINTER attribute");
      goto cleanup;
    }

  m = init_flag ? gfc_match_init_expr (&e1) : gfc_match_expr (&e1);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_char (',') != MATCH_YES)
    goto syntax;

  m = init_flag ? gfc_match_init_expr (&e2) : gfc_match_expr (&e2);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_char (',') != MATCH_YES)
    {
      e3 = gfc_int_expr (1);
      goto done;
    }

  m = init_flag ? gfc_match_init_expr (&e3) : gfc_match_expr (&e3);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      gfc_error ("Expected a step value in iterator at %C");
      goto cleanup;
    }

done:
  iter->var = var;
  iter->start = e1;
  iter->end = e2;
  iter->step = e3;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in iterator at %C");

cleanup:
  gfc_free_expr (e1);
  gfc_free_expr (e2);
  gfc_free_expr (e3);

  return MATCH_ERROR;
}


/* Tries to match the next non-whitespace character on the input.
   This subroutine does not return MATCH_ERROR.  */

match
gfc_match_char (char c)
{
  locus where;

  where = gfc_current_locus;
  gfc_gobble_whitespace ();

  if (gfc_next_char () == c)
    return MATCH_YES;

  gfc_current_locus = where;
  return MATCH_NO;
}


/* General purpose matching subroutine.  The target string is a
   scanf-like format string in which spaces correspond to arbitrary
   whitespace (including no whitespace), characters correspond to
   themselves.  The %-codes are:

   %%  Literal percent sign
   %e  Expression, pointer to a pointer is set
   %s  Symbol, pointer to the symbol is set
   %n  Name, character buffer is set to name
   %t  Matches end of statement.
   %o  Matches an intrinsic operator, returned as an INTRINSIC enum.
   %l  Matches a statement label
   %v  Matches a variable expression (an lvalue)
   %   Matches a required space (in free form) and optional spaces.  */

match
gfc_match (const char *target, ...)
{
  gfc_st_label **label;
  int matches, *ip;
  locus old_loc;
  va_list argp;
  char c, *np;
  match m, n;
  void **vp;
  const char *p;

  old_loc = gfc_current_locus;
  va_start (argp, target);
  m = MATCH_NO;
  matches = 0;
  p = target;

loop:
  c = *p++;
  switch (c)
    {
    case ' ':
      gfc_gobble_whitespace ();
      goto loop;
    case '\0':
      m = MATCH_YES;
      break;

    case '%':
      c = *p++;
      switch (c)
	{
	case 'e':
	  vp = va_arg (argp, void **);
	  n = gfc_match_expr ((gfc_expr **) vp);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 'v':
	  vp = va_arg (argp, void **);
	  n = gfc_match_variable ((gfc_expr **) vp, 0);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 's':
	  vp = va_arg (argp, void **);
	  n = gfc_match_symbol ((gfc_symbol **) vp, 0);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 'n':
	  np = va_arg (argp, char *);
	  n = gfc_match_name (np);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 'l':
	  label = va_arg (argp, gfc_st_label **);
	  n = gfc_match_st_label (label, 0);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 'o':
	  ip = va_arg (argp, int *);
	  n = gfc_match_intrinsic_op ((gfc_intrinsic_op *) ip);
	  if (n != MATCH_YES)
	    {
	      m = n;
	      goto not_yes;
	    }

	  matches++;
	  goto loop;

	case 't':
	  if (gfc_match_eos () != MATCH_YES)
	    {
	      m = MATCH_NO;
	      goto not_yes;
	    }
	  goto loop;

	case ' ':
	  if (gfc_match_space () == MATCH_YES)
	    goto loop;
	  m = MATCH_NO;
	  goto not_yes;

	case '%':
	  break;	/* Fall through to character matcher */

	default:
	  gfc_internal_error ("gfc_match(): Bad match code %c", c);
	}

    default:
      if (c == gfc_next_char ())
	goto loop;
      break;
    }

not_yes:
  va_end (argp);

  if (m != MATCH_YES)
    {
      /* Clean up after a failed match.  */
      gfc_current_locus = old_loc;
      va_start (argp, target);

      p = target;
      for (; matches > 0; matches--)
	{
	  while (*p++ != '%');

	  switch (*p++)
	    {
	    case '%':
	      matches++;
	      break;		/* Skip */

	    /* Matches that don't have to be undone */
	    case 'o':
	    case 'l':
	    case 'n':
	    case 's':
	      (void)va_arg (argp, void **);
	      break;

	    case 'e':
	    case 'v':
	      vp = va_arg (argp, void **);
	      gfc_free_expr (*vp);
	      *vp = NULL;
	      break;
	    }
	}

      va_end (argp);
    }

  return m;
}


/*********************** Statement level matching **********************/

/* Matches the start of a program unit, which is the program keyword
   followed by an obligatory symbol.  */

match
gfc_match_program (void)
{
  gfc_symbol *sym;
  match m;

  m = gfc_match ("% %s%t", &sym);

  if (m == MATCH_NO)
    {
      gfc_error ("Invalid form of PROGRAM statement at %C");
      m = MATCH_ERROR;
    }

  if (m == MATCH_ERROR)
    return m;

  if (gfc_add_flavor (&sym->attr, FL_PROGRAM, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Match a simple assignment statement.  */

match
gfc_match_assignment (void)
{
  gfc_expr *lvalue, *rvalue;
  locus old_loc;
  match m;

  old_loc = gfc_current_locus;

  lvalue = rvalue = NULL;
  m = gfc_match (" %v =", &lvalue);
  if (m != MATCH_YES)
    goto cleanup;

  if (lvalue->symtree->n.sym->attr.flavor == FL_PARAMETER)
    {
      gfc_error ("Cannot assign to a PARAMETER variable at %C");
      m = MATCH_ERROR;
      goto cleanup;
    }

  m = gfc_match (" %e%t", &rvalue);
  if (m != MATCH_YES)
    goto cleanup;

  gfc_set_sym_referenced (lvalue->symtree->n.sym);

  new_st.op = EXEC_ASSIGN;
  new_st.expr = lvalue;
  new_st.expr2 = rvalue;

  gfc_check_do_variable (lvalue->symtree);

  return MATCH_YES;

cleanup:
  gfc_current_locus = old_loc;
  gfc_free_expr (lvalue);
  gfc_free_expr (rvalue);
  return m;
}


/* Match a pointer assignment statement.  */

match
gfc_match_pointer_assignment (void)
{
  gfc_expr *lvalue, *rvalue;
  locus old_loc;
  match m;

  old_loc = gfc_current_locus;

  lvalue = rvalue = NULL;

  m = gfc_match (" %v =>", &lvalue);
  if (m != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  m = gfc_match (" %e%t", &rvalue);
  if (m != MATCH_YES)
    goto cleanup;

  new_st.op = EXEC_POINTER_ASSIGN;
  new_st.expr = lvalue;
  new_st.expr2 = rvalue;

  return MATCH_YES;

cleanup:
  gfc_current_locus = old_loc;
  gfc_free_expr (lvalue);
  gfc_free_expr (rvalue);
  return m;
}


/* We try to match an easy arithmetic IF statement. This only happens
   when just after having encountered a simple IF statement. This code
   is really duplicate with parts of the gfc_match_if code, but this is
   *much* easier.  */
static match
match_arithmetic_if (void)
{
  gfc_st_label *l1, *l2, *l3;
  gfc_expr *expr;
  match m;

  m = gfc_match (" ( %e ) %l , %l , %l%t", &expr, &l1, &l2, &l3);
  if (m != MATCH_YES)
    return m;

  if (gfc_reference_st_label (l1, ST_LABEL_TARGET) == FAILURE
      || gfc_reference_st_label (l2, ST_LABEL_TARGET) == FAILURE
      || gfc_reference_st_label (l3, ST_LABEL_TARGET) == FAILURE)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  if (gfc_notify_std (GFC_STD_F95_DEL,
		      "Obsolete: arithmetic IF statement at %C") == FAILURE)
    return MATCH_ERROR;

  new_st.op = EXEC_ARITHMETIC_IF;
  new_st.expr = expr;
  new_st.label = l1;
  new_st.label2 = l2;
  new_st.label3 = l3;

  return MATCH_YES;
}


/* The IF statement is a bit of a pain.  First of all, there are three
   forms of it, the simple IF, the IF that starts a block and the
   arithmetic IF.

   There is a problem with the simple IF and that is the fact that we
   only have a single level of undo information on symbols.  What this
   means is for a simple IF, we must re-match the whole IF statement
   multiple times in order to guarantee that the symbol table ends up
   in the proper state.  */

static match match_simple_forall (void);
static match match_simple_where (void);

match
gfc_match_if (gfc_statement * if_type)
{
  gfc_expr *expr;
  gfc_st_label *l1, *l2, *l3;
  locus old_loc;
  gfc_code *p;
  match m, n;

  n = gfc_match_label ();
  if (n == MATCH_ERROR)
    return n;

  old_loc = gfc_current_locus;

  m = gfc_match (" if ( %e", &expr);
  if (m != MATCH_YES)
    return m;

  if (gfc_match_char (')') != MATCH_YES)
    {
      gfc_error ("Syntax error in IF-expression at %C");
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  m = gfc_match (" %l , %l , %l%t", &l1, &l2, &l3);

  if (m == MATCH_YES)
    {
      if (n == MATCH_YES)
	{
	  gfc_error
	    ("Block label not appropriate for arithmetic IF statement "
	     "at %C");

	  gfc_free_expr (expr);
	  return MATCH_ERROR;
	}

      if (gfc_reference_st_label (l1, ST_LABEL_TARGET) == FAILURE
	  || gfc_reference_st_label (l2, ST_LABEL_TARGET) == FAILURE
	  || gfc_reference_st_label (l3, ST_LABEL_TARGET) == FAILURE)
	{

	  gfc_free_expr (expr);
	  return MATCH_ERROR;
	}
      
      if (gfc_notify_std (GFC_STD_F95_DEL,
  		          "Obsolete: arithmetic IF statement at %C")
	  == FAILURE)
        return MATCH_ERROR;

      new_st.op = EXEC_ARITHMETIC_IF;
      new_st.expr = expr;
      new_st.label = l1;
      new_st.label2 = l2;
      new_st.label3 = l3;

      *if_type = ST_ARITHMETIC_IF;
      return MATCH_YES;
    }

  if (gfc_match (" then%t") == MATCH_YES)
    {
      new_st.op = EXEC_IF;
      new_st.expr = expr;

      *if_type = ST_IF_BLOCK;
      return MATCH_YES;
    }

  if (n == MATCH_YES)
    {
      gfc_error ("Block label is not appropriate IF statement at %C");

      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  /* At this point the only thing left is a simple IF statement.  At
     this point, n has to be MATCH_NO, so we don't have to worry about
     re-matching a block label.  From what we've got so far, try
     matching an assignment.  */

  *if_type = ST_SIMPLE_IF;

  m = gfc_match_assignment ();
  if (m == MATCH_YES)
    goto got_match;

  gfc_free_expr (expr);
  gfc_undo_symbols ();
  gfc_current_locus = old_loc;

  gfc_match (" if ( %e ) ", &expr);	/* Guaranteed to match */

  m = gfc_match_pointer_assignment ();
  if (m == MATCH_YES)
    goto got_match;

  gfc_free_expr (expr);
  gfc_undo_symbols ();
  gfc_current_locus = old_loc;

  gfc_match (" if ( %e ) ", &expr);	/* Guaranteed to match */

  /* Look at the next keyword to see which matcher to call.  Matching
     the keyword doesn't affect the symbol table, so we don't have to
     restore between tries.  */

#define match(string, subr, statement) \
  if (gfc_match(string) == MATCH_YES) { m = subr(); goto got_match; }

  gfc_clear_error ();

  match ("allocate", gfc_match_allocate, ST_ALLOCATE)
    match ("assign", gfc_match_assign, ST_LABEL_ASSIGNMENT)
    match ("backspace", gfc_match_backspace, ST_BACKSPACE)
    match ("call", gfc_match_call, ST_CALL)
    match ("close", gfc_match_close, ST_CLOSE)
    match ("continue", gfc_match_continue, ST_CONTINUE)
    match ("cycle", gfc_match_cycle, ST_CYCLE)
    match ("deallocate", gfc_match_deallocate, ST_DEALLOCATE)
    match ("end file", gfc_match_endfile, ST_END_FILE)
    match ("exit", gfc_match_exit, ST_EXIT)
    match ("forall", match_simple_forall, ST_FORALL)
    match ("go to", gfc_match_goto, ST_GOTO)
    match ("if", match_arithmetic_if, ST_ARITHMETIC_IF)
    match ("inquire", gfc_match_inquire, ST_INQUIRE)
    match ("nullify", gfc_match_nullify, ST_NULLIFY)
    match ("open", gfc_match_open, ST_OPEN)
    match ("pause", gfc_match_pause, ST_NONE)
    match ("print", gfc_match_print, ST_WRITE)
    match ("read", gfc_match_read, ST_READ)
    match ("return", gfc_match_return, ST_RETURN)
    match ("rewind", gfc_match_rewind, ST_REWIND)
    match ("stop", gfc_match_stop, ST_STOP)
    match ("where", match_simple_where, ST_WHERE)
    match ("write", gfc_match_write, ST_WRITE)

  /* All else has failed, so give up.  See if any of the matchers has
     stored an error message of some sort.  */
    if (gfc_error_check () == 0)
    gfc_error ("Unclassifiable statement in IF-clause at %C");

  gfc_free_expr (expr);
  return MATCH_ERROR;

got_match:
  if (m == MATCH_NO)
    gfc_error ("Syntax error in IF-clause at %C");
  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  /* At this point, we've matched the single IF and the action clause
     is in new_st.  Rearrange things so that the IF statement appears
     in new_st.  */

  p = gfc_get_code ();
  p->next = gfc_get_code ();
  *p->next = new_st;
  p->next->loc = gfc_current_locus;

  p->expr = expr;
  p->op = EXEC_IF;

  gfc_clear_new_st ();

  new_st.op = EXEC_IF;
  new_st.block = p;

  return MATCH_YES;
}

#undef match


/* Match an ELSE statement.  */

match
gfc_match_else (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

  if (gfc_match_name (name) != MATCH_YES
      || gfc_current_block () == NULL
      || gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after ELSE statement at %C");
      return MATCH_ERROR;
    }

  if (strcmp (name, gfc_current_block ()->name) != 0)
    {
      gfc_error ("Label '%s' at %C doesn't match IF label '%s'",
		 name, gfc_current_block ()->name);
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Match an ELSE IF statement.  */

match
gfc_match_elseif (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *expr;
  match m;

  m = gfc_match (" ( %e ) then", &expr);
  if (m != MATCH_YES)
    return m;

  if (gfc_match_eos () == MATCH_YES)
    goto done;

  if (gfc_match_name (name) != MATCH_YES
      || gfc_current_block () == NULL
      || gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after ELSE IF statement at %C");
      goto cleanup;
    }

  if (strcmp (name, gfc_current_block ()->name) != 0)
    {
      gfc_error ("Label '%s' at %C doesn't match IF label '%s'",
		 name, gfc_current_block ()->name);
      goto cleanup;
    }

done:
  new_st.op = EXEC_IF;
  new_st.expr = expr;
  return MATCH_YES;

cleanup:
  gfc_free_expr (expr);
  return MATCH_ERROR;
}


/* Free a gfc_iterator structure.  */

void
gfc_free_iterator (gfc_iterator * iter, int flag)
{

  if (iter == NULL)
    return;

  gfc_free_expr (iter->var);
  gfc_free_expr (iter->start);
  gfc_free_expr (iter->end);
  gfc_free_expr (iter->step);

  if (flag)
    gfc_free (iter);
}


/* Match a DO statement.  */

match
gfc_match_do (void)
{
  gfc_iterator iter, *ip;
  locus old_loc;
  gfc_st_label *label;
  match m;

  old_loc = gfc_current_locus;

  label = NULL;
  iter.var = iter.start = iter.end = iter.step = NULL;

  m = gfc_match_label ();
  if (m == MATCH_ERROR)
    return m;

  if (gfc_match (" do") != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_st_label (&label, 0);
  if (m == MATCH_ERROR)
    goto cleanup;

/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */

  if (gfc_match_eos () == MATCH_YES)
    {
      iter.end = gfc_logical_expr (1, NULL);
      new_st.op = EXEC_DO_WHILE;
      goto done;
    }

  /* match an optional comma, if no comma is found a space is obligatory.  */
  if (gfc_match_char(',') != MATCH_YES
      && gfc_match ("% ") != MATCH_YES)
    return MATCH_NO;

  /* See if we have a DO WHILE.  */
  if (gfc_match (" while ( %e )%t", &iter.end) == MATCH_YES)
    {
      new_st.op = EXEC_DO_WHILE;
      goto done;
    }

  /* The abortive DO WHILE may have done something to the symbol
     table, so we start over: */
  gfc_undo_symbols ();
  gfc_current_locus = old_loc;

  gfc_match_label ();		/* This won't error */
  gfc_match (" do ");		/* This will work */

  gfc_match_st_label (&label, 0);	/* Can't error out */
  gfc_match_char (',');		/* Optional comma */

  m = gfc_match_iterator (&iter, 0);
  if (m == MATCH_NO)
    return MATCH_NO;
  if (m == MATCH_ERROR)
    goto cleanup;

  gfc_check_do_variable (iter.var->symtree);

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_DO);
      goto cleanup;
    }

  new_st.op = EXEC_DO;

done:
  if (label != NULL
      && gfc_reference_st_label (label, ST_LABEL_TARGET) == FAILURE)
    goto cleanup;

  new_st.label = label;

  if (new_st.op == EXEC_DO_WHILE)
    new_st.expr = iter.end;
  else
    {
      new_st.ext.iterator = ip = gfc_get_iterator ();
      *ip = iter;
    }

  return MATCH_YES;

cleanup:
  gfc_free_iterator (&iter, 0);

  return MATCH_ERROR;
}


/* Match an EXIT or CYCLE statement.  */

static match
match_exit_cycle (gfc_statement st, gfc_exec_op op)
{
  gfc_state_data *p;
  gfc_symbol *sym;
  match m;

  if (gfc_match_eos () == MATCH_YES)
    sym = NULL;
  else
    {
      m = gfc_match ("% %s%t", &sym);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;
      if (m == MATCH_NO)
	{
	  gfc_syntax_error (st);
	  return MATCH_ERROR;
	}

      if (sym->attr.flavor != FL_LABEL)
	{
	  gfc_error ("Name '%s' in %s statement at %C is not a loop name",
		     sym->name, gfc_ascii_statement (st));
	  return MATCH_ERROR;
	}
    }

  /* Find the loop mentioned specified by the label (or lack of a
     label).  */
  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_DO && (sym == NULL || sym == p->sym))
      break;

  if (p == NULL)
    {
      if (sym == NULL)
	gfc_error ("%s statement at %C is not within a loop",
		   gfc_ascii_statement (st));
      else
	gfc_error ("%s statement at %C is not within loop '%s'",
		   gfc_ascii_statement (st), sym->name);

      return MATCH_ERROR;
    }

  /* Save the first statement in the loop - needed by the backend.  */
  new_st.ext.whichloop = p->head;

  new_st.op = op;
/*  new_st.sym = sym;*/

  return MATCH_YES;
}


/* Match the EXIT statement.  */

match
gfc_match_exit (void)
{

  return match_exit_cycle (ST_EXIT, EXEC_EXIT);
}


/* Match the CYCLE statement.  */

match
gfc_match_cycle (void)
{

  return match_exit_cycle (ST_CYCLE, EXEC_CYCLE);
}


/* Match a number or character constant after a STOP or PAUSE statement.  */

static match
gfc_match_stopcode (gfc_statement st)
{
  int stop_code;
  gfc_expr *e;
  match m;

  stop_code = 0;
  e = NULL;

  if (gfc_match_eos () != MATCH_YES)
    {
      m = gfc_match_small_literal_int (&stop_code);
      if (m == MATCH_ERROR)
        goto cleanup;

      if (m == MATCH_YES && stop_code > 99999)
        {
          gfc_error ("STOP code out of range at %C");
          goto cleanup;
        }

      if (m == MATCH_NO)
        {
          /* Try a character constant.  */
          m = gfc_match_expr (&e);
          if (m == MATCH_ERROR)
            goto cleanup;
          if (m == MATCH_NO)
            goto syntax;
          if (e->ts.type != BT_CHARACTER || e->expr_type != EXPR_CONSTANT)
            goto syntax;
        }

      if (gfc_match_eos () != MATCH_YES)
        goto syntax;
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("%s statement not allowed in PURE procedure at %C",
	         gfc_ascii_statement (st));
      goto cleanup;
    }

  new_st.op = st == ST_STOP ? EXEC_STOP : EXEC_PAUSE;
  new_st.expr = e;
  new_st.ext.stop_code = stop_code;

  return MATCH_YES;

syntax:
  gfc_syntax_error (st);

cleanup:

  gfc_free_expr (e);
  return MATCH_ERROR;
}

/* Match the (deprecated) PAUSE statement.  */

match
gfc_match_pause (void)
{
  match m;

  m = gfc_match_stopcode (ST_PAUSE);
  if (m == MATCH_YES)
    {
      if (gfc_notify_std (GFC_STD_F95_DEL,
	    "Obsolete: PAUSE statement at %C")
	  == FAILURE)
	m = MATCH_ERROR;
    }
  return m;
}


/* Match the STOP statement.  */

match
gfc_match_stop (void)
{
  return gfc_match_stopcode (ST_STOP);
}


/* Match a CONTINUE statement.  */

match
gfc_match_continue (void)
{

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_CONTINUE);
      return MATCH_ERROR;
    }

  new_st.op = EXEC_CONTINUE;
  return MATCH_YES;
}


/* Match the (deprecated) ASSIGN statement.  */

match
gfc_match_assign (void)
{
  gfc_expr *expr;
  gfc_st_label *label;

  if (gfc_match (" %l", &label) == MATCH_YES)
    {
      if (gfc_reference_st_label (label, ST_LABEL_UNKNOWN) == FAILURE)
        return MATCH_ERROR;
      if (gfc_match (" to %v%t", &expr) == MATCH_YES)
        {
	  if (gfc_notify_std (GFC_STD_F95_DEL,
		"Obsolete: ASSIGN statement at %C")
	      == FAILURE)
	    return MATCH_ERROR;

          expr->symtree->n.sym->attr.assign = 1;

          new_st.op = EXEC_LABEL_ASSIGN;
          new_st.label = label;
          new_st.expr = expr;
          return MATCH_YES;
        }
    }
  return MATCH_NO;
}


/* Match the GO TO statement.  As a computed GOTO statement is
   matched, it is transformed into an equivalent SELECT block.  No
   tree is necessary, and the resulting jumps-to-jumps are
   specifically optimized away by the back end.  */

match
gfc_match_goto (void)
{
  gfc_code *head, *tail;
  gfc_expr *expr;
  gfc_case *cp;
  gfc_st_label *label;
  int i;
  match m;

  if (gfc_match (" %l%t", &label) == MATCH_YES)
    {
      if (gfc_reference_st_label (label, ST_LABEL_TARGET) == FAILURE)
	return MATCH_ERROR;

      new_st.op = EXEC_GOTO;
      new_st.label = label;
      return MATCH_YES;
    }

  /* The assigned GO TO statement.  */ 

  if (gfc_match_variable (&expr, 0) == MATCH_YES)
    {
      if (gfc_notify_std (GFC_STD_F95_DEL,
			  "Obsolete: Assigned GOTO statement at %C")
	  == FAILURE)
	return MATCH_ERROR;

      new_st.op = EXEC_GOTO;
      new_st.expr = expr;

      if (gfc_match_eos () == MATCH_YES)
	return MATCH_YES;

      /* Match label list.  */
      gfc_match_char (',');
      if (gfc_match_char ('(') != MATCH_YES)
	{
	  gfc_syntax_error (ST_GOTO);
	  return MATCH_ERROR;
	}
      head = tail = NULL;

      do
	{
	  m = gfc_match_st_label (&label, 0);
	  if (m != MATCH_YES)
	    goto syntax;

	  if (gfc_reference_st_label (label, ST_LABEL_TARGET) == FAILURE)
	    goto cleanup;

	  if (head == NULL)
	    head = tail = gfc_get_code ();
	  else
	    {
	      tail->block = gfc_get_code ();
	      tail = tail->block;
	    }

	  tail->label = label;
	  tail->op = EXEC_GOTO;
	}
      while (gfc_match_char (',') == MATCH_YES);

      if (gfc_match (")%t") != MATCH_YES)
	goto syntax;

      if (head == NULL)
	{
	   gfc_error (
	       "Statement label list in GOTO at %C cannot be empty");
	   goto syntax;
	}
      new_st.block = head;

      return MATCH_YES;
    }

  /* Last chance is a computed GO TO statement.  */
  if (gfc_match_char ('(') != MATCH_YES)
    {
      gfc_syntax_error (ST_GOTO);
      return MATCH_ERROR;
    }

  head = tail = NULL;
  i = 1;

  do
    {
      m = gfc_match_st_label (&label, 0);
      if (m != MATCH_YES)
	goto syntax;

      if (gfc_reference_st_label (label, ST_LABEL_TARGET) == FAILURE)
	goto cleanup;

      if (head == NULL)
	head = tail = gfc_get_code ();
      else
	{
	  tail->block = gfc_get_code ();
	  tail = tail->block;
	}

      cp = gfc_get_case ();
      cp->low = cp->high = gfc_int_expr (i++);

      tail->op = EXEC_SELECT;
      tail->ext.case_list = cp;

      tail->next = gfc_get_code ();
      tail->next->op = EXEC_GOTO;
      tail->next->label = label;
    }
  while (gfc_match_char (',') == MATCH_YES);

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;

  if (head == NULL)
    {
      gfc_error ("Statement label list in GOTO at %C cannot be empty");
      goto syntax;
    }

  /* Get the rest of the statement.  */
  gfc_match_char (',');

  if (gfc_match (" %e%t", &expr) != MATCH_YES)
    goto syntax;

  /* At this point, a computed GOTO has been fully matched and an
     equivalent SELECT statement constructed.  */

  new_st.op = EXEC_SELECT;
  new_st.expr = NULL;

  /* Hack: For a "real" SELECT, the expression is in expr. We put
     it in expr2 so we can distinguish then and produce the correct
     diagnostics.  */
  new_st.expr2 = expr;
  new_st.block = head;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_GOTO);
cleanup:
  gfc_free_statements (head);
  return MATCH_ERROR;
}


/* Frees a list of gfc_alloc structures.  */

void
gfc_free_alloc_list (gfc_alloc * p)
{
  gfc_alloc *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free_expr (p->expr);
      gfc_free (p);
    }
}


/* Match an ALLOCATE statement.  */

match
gfc_match_allocate (void)
{
  gfc_alloc *head, *tail;
  gfc_expr *stat;
  match m;

  head = tail = NULL;
  stat = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  for (;;)
    {
      if (head == NULL)
	head = tail = gfc_get_alloc ();
      else
	{
	  tail->next = gfc_get_alloc ();
	  tail = tail->next;
	}

      m = gfc_match_variable (&tail->expr, 0);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      if (gfc_check_do_variable (tail->expr->symtree))
	goto cleanup;

      if (gfc_pure (NULL)
          && gfc_impure_variable (tail->expr->symtree->n.sym))
	{
	  gfc_error ("Bad allocate-object in ALLOCATE statement at %C for a "
		     "PURE procedure");
	  goto cleanup;
	}

      if (gfc_match_char (',') != MATCH_YES)
	break;

      m = gfc_match (" stat = %v", &stat);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	break;
    }

  if (stat != NULL)
    {
      if (stat->symtree->n.sym->attr.intent == INTENT_IN)
	{
	  gfc_error
	    ("STAT variable '%s' of ALLOCATE statement at %C cannot be "
	     "INTENT(IN)", stat->symtree->n.sym->name);
	  goto cleanup;
	}

      if (gfc_pure (NULL) && gfc_impure_variable (stat->symtree->n.sym))
	{
	  gfc_error
	    ("Illegal STAT variable in ALLOCATE statement at %C for a PURE "
	     "procedure");
	  goto cleanup;
	}

      if (stat->symtree->n.sym->attr.flavor != FL_VARIABLE)
	{
	  gfc_error("STAT expression at %C must be a variable");
	  goto cleanup;
	}

      gfc_check_do_variable(stat->symtree);
    }

  if (gfc_match (" )%t") != MATCH_YES)
    goto syntax;

  new_st.op = EXEC_ALLOCATE;
  new_st.expr = stat;
  new_st.ext.alloc_list = head;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_ALLOCATE);

cleanup:
  gfc_free_expr (stat);
  gfc_free_alloc_list (head);
  return MATCH_ERROR;
}


/* Match a NULLIFY statement. A NULLIFY statement is transformed into
   a set of pointer assignments to intrinsic NULL().  */

match
gfc_match_nullify (void)
{
  gfc_code *tail;
  gfc_expr *e, *p;
  match m;

  tail = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  for (;;)
    {
      m = gfc_match_variable (&p, 0);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      if (gfc_check_do_variable(p->symtree))
	goto cleanup;

      if (gfc_pure (NULL) && gfc_impure_variable (p->symtree->n.sym))
	{
	  gfc_error
	    ("Illegal variable in NULLIFY at %C for a PURE procedure");
	  goto cleanup;
	}

      /* build ' => NULL() ' */
      e = gfc_get_expr ();
      e->where = gfc_current_locus;
      e->expr_type = EXPR_NULL;
      e->ts.type = BT_UNKNOWN;

      /* Chain to list */
      if (tail == NULL)
	tail = &new_st;
      else
	{
	  tail->next = gfc_get_code ();
	  tail = tail->next;
	}

      tail->op = EXEC_POINTER_ASSIGN;
      tail->expr = p;
      tail->expr2 = e;

      if (gfc_match (" )%t") == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_NULLIFY);

cleanup:
  gfc_free_statements (tail);
  return MATCH_ERROR;
}


/* Match a DEALLOCATE statement.  */

match
gfc_match_deallocate (void)
{
  gfc_alloc *head, *tail;
  gfc_expr *stat;
  match m;

  head = tail = NULL;
  stat = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  for (;;)
    {
      if (head == NULL)
	head = tail = gfc_get_alloc ();
      else
	{
	  tail->next = gfc_get_alloc ();
	  tail = tail->next;
	}

      m = gfc_match_variable (&tail->expr, 0);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      if (gfc_check_do_variable (tail->expr->symtree))
	goto cleanup;

      if (gfc_pure (NULL)
          && gfc_impure_variable (tail->expr->symtree->n.sym))
	{
	  gfc_error
	    ("Illegal deallocate-expression in DEALLOCATE at %C for a PURE "
	     "procedure");
	  goto cleanup;
	}

      if (gfc_match_char (',') != MATCH_YES)
	break;

      m = gfc_match (" stat = %v", &stat);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	break;
    }

  if (stat != NULL)
    {
      if (stat->symtree->n.sym->attr.intent == INTENT_IN)
	{
	  gfc_error ("STAT variable '%s' of DEALLOCATE statement at %C "
		     "cannot be INTENT(IN)", stat->symtree->n.sym->name);
	  goto cleanup;
	}

      if (gfc_pure(NULL) && gfc_impure_variable (stat->symtree->n.sym))
	{
	  gfc_error ("Illegal STAT variable in DEALLOCATE statement at %C "
		     "for a PURE procedure");
	  goto cleanup;
	}

      if (stat->symtree->n.sym->attr.flavor != FL_VARIABLE)
	{
	  gfc_error("STAT expression at %C must be a variable");
	  goto cleanup;
	}

      gfc_check_do_variable(stat->symtree);
    }

  if (gfc_match (" )%t") != MATCH_YES)
    goto syntax;

  new_st.op = EXEC_DEALLOCATE;
  new_st.expr = stat;
  new_st.ext.alloc_list = head;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DEALLOCATE);

cleanup:
  gfc_free_expr (stat);
  gfc_free_alloc_list (head);
  return MATCH_ERROR;
}


/* Match a RETURN statement.  */

match
gfc_match_return (void)
{
  gfc_expr *e;
  match m;
  gfc_compile_state s;
  int c;

  e = NULL;
  if (gfc_match_eos () == MATCH_YES)
    goto done;

  if (gfc_find_state (COMP_SUBROUTINE) == FAILURE)
    {
      gfc_error ("Alternate RETURN statement at %C is only allowed within "
		 "a SUBROUTINE");
      goto cleanup;
    }

  if (gfc_current_form == FORM_FREE)
    {
      /* The following are valid, so we can't require a blank after the
        RETURN keyword:
          return+1
          return(1)  */
      c = gfc_peek_char ();
      if (ISALPHA (c) || ISDIGIT (c))
       return MATCH_NO;
    }

  m = gfc_match (" %e%t", &e);
  if (m == MATCH_YES)
    goto done;
  if (m == MATCH_ERROR)
    goto cleanup;

  gfc_syntax_error (ST_RETURN);

cleanup:
  gfc_free_expr (e);
  return MATCH_ERROR;

done:
  gfc_enclosing_unit (&s);
  if (s == COMP_PROGRAM
      && gfc_notify_std (GFC_STD_GNU, "Extension: RETURN statement in "
                        "main program at %C") == FAILURE)
      return MATCH_ERROR;

  new_st.op = EXEC_RETURN;
  new_st.expr = e;

  return MATCH_YES;
}


/* Match a CALL statement.  The tricky part here are possible
   alternate return specifiers.  We handle these by having all
   "subroutines" actually return an integer via a register that gives
   the return number.  If the call specifies alternate returns, we
   generate code for a SELECT statement whose case clauses contain
   GOTOs to the various labels.  */

match
gfc_match_call (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_actual_arglist *a, *arglist;
  gfc_case *new_case;
  gfc_symbol *sym;
  gfc_symtree *st;
  gfc_code *c;
  match m;
  int i;

  arglist = NULL;

  m = gfc_match ("% %n", name);
  if (m == MATCH_NO)
    goto syntax;
  if (m != MATCH_YES)
    return m;

  if (gfc_get_ha_sym_tree (name, &st))
    return MATCH_ERROR;

  sym = st->n.sym;
  gfc_set_sym_referenced (sym);

  if (!sym->attr.generic
      && !sym->attr.subroutine
      && gfc_add_subroutine (&sym->attr, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  if (gfc_match_eos () != MATCH_YES)
    {
      m = gfc_match_actual_arglist (1, &arglist);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      if (gfc_match_eos () != MATCH_YES)
	goto syntax;
    }

  /* If any alternate return labels were found, construct a SELECT
     statement that will jump to the right place.  */

  i = 0;
  for (a = arglist; a; a = a->next)
    if (a->expr == NULL)
	i = 1;

  if (i)
    {
      gfc_symtree *select_st;
      gfc_symbol *select_sym;
      char name[GFC_MAX_SYMBOL_LEN + 1];

      new_st.next = c = gfc_get_code ();
      c->op = EXEC_SELECT;
      sprintf (name, "_result_%s",sym->name);
      gfc_get_ha_sym_tree (name, &select_st);  /* Can't fail */

      select_sym = select_st->n.sym;
      select_sym->ts.type = BT_INTEGER;
      select_sym->ts.kind = gfc_default_integer_kind;
      gfc_set_sym_referenced (select_sym);
      c->expr = gfc_get_expr ();
      c->expr->expr_type = EXPR_VARIABLE;
      c->expr->symtree = select_st;
      c->expr->ts = select_sym->ts;
      c->expr->where = gfc_current_locus;

      i = 0;
      for (a = arglist; a; a = a->next)
	{
	  if (a->expr != NULL)
	    continue;

	  if (gfc_reference_st_label (a->label, ST_LABEL_TARGET) == FAILURE)
	    continue;

	  i++;

	  c->block = gfc_get_code ();
	  c = c->block;
	  c->op = EXEC_SELECT;

	  new_case = gfc_get_case ();
	  new_case->high = new_case->low = gfc_int_expr (i);
	  c->ext.case_list = new_case;

	  c->next = gfc_get_code ();
	  c->next->op = EXEC_GOTO;
	  c->next->label = a->label;
	}
    }

  new_st.op = EXEC_CALL;
  new_st.symtree = st;
  new_st.ext.actual = arglist;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_CALL);

cleanup:
  gfc_free_actual_arglist (arglist);
  return MATCH_ERROR;
}


/* Given a name, return a pointer to the common head structure,
   creating it if it does not exist. If FROM_MODULE is nonzero, we
   mangle the name so that it doesn't interfere with commons defined 
   in the using namespace.
   TODO: Add to global symbol tree.  */

gfc_common_head *
gfc_get_common (const char *name, int from_module)
{
  gfc_symtree *st;
  static int serial = 0;
  char mangled_name[GFC_MAX_SYMBOL_LEN+1];

  if (from_module)
    {
      /* A use associated common block is only needed to correctly layout
	 the variables it contains.  */
      snprintf(mangled_name, GFC_MAX_SYMBOL_LEN, "_%d_%s", serial++, name);
      st = gfc_new_symtree (&gfc_current_ns->common_root, mangled_name);
    }
  else
    {
      st = gfc_find_symtree (gfc_current_ns->common_root, name);

      if (st == NULL)
	st = gfc_new_symtree (&gfc_current_ns->common_root, name);
    }

  if (st->n.common == NULL)
    {
      st->n.common = gfc_get_common_head ();
      st->n.common->where = gfc_current_locus;
      strcpy (st->n.common->name, name);
    }

  return st->n.common;
}


/* Match a common block name.  */

static match
match_common_name (char *name)
{
  match m;

  if (gfc_match_char ('/') == MATCH_NO)
    {
      name[0] = '\0';
      return MATCH_YES;
    }

  if (gfc_match_char ('/') == MATCH_YES)
    {
      name[0] = '\0';
      return MATCH_YES;
    }

  m = gfc_match_name (name);

  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (m == MATCH_YES && gfc_match_char ('/') == MATCH_YES)
    return MATCH_YES;

  gfc_error ("Syntax error in common block name at %C");
  return MATCH_ERROR;
}


/* Match a COMMON statement.  */

match
gfc_match_common (void)
{
  gfc_symbol *sym, **head, *tail, *old_blank_common;
  char name[GFC_MAX_SYMBOL_LEN+1];
  gfc_common_head *t;
  gfc_array_spec *as;
  match m;

  old_blank_common = gfc_current_ns->blank_common.head;
  if (old_blank_common)
    {
      while (old_blank_common->common_next)
	old_blank_common = old_blank_common->common_next;
    }

  as = NULL;

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for (;;)
    {
      m = match_common_name (name);
      if (m == MATCH_ERROR)
	goto cleanup;

      if (name[0] == '\0')
	{
	  t = &gfc_current_ns->blank_common;
	  if (t->head == NULL)
	    t->where = gfc_current_locus;
	  head = &t->head;
	}
      else
	{
	  t = gfc_get_common (name, 0);
	  head = &t->head;
	}

      if (*head == NULL)
	tail = NULL;
      else
	{
	  tail = *head;
	  while (tail->common_next)
	    tail = tail->common_next;
	}

      /* Grab the list of symbols.  */
      if (gfc_match_eos () == MATCH_YES)
	goto done;
  
      for (;;)
	{
	  m = gfc_match_symbol (&sym, 0);
	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    goto syntax;

	  if (sym->attr.in_common)
	    {
	      gfc_error ("Symbol '%s' at %C is already in a COMMON block",
			 sym->name);
	      goto cleanup;
	    }

	  if (gfc_add_in_common (&sym->attr, sym->name, NULL) == FAILURE) 
	    goto cleanup;

	  if (sym->value != NULL
	      && (name[0] == '\0' || !sym->attr.data))
	    {
	      if (name[0] == '\0')
		gfc_error ("Previously initialized symbol '%s' in "
			   "blank COMMON block at %C", sym->name);
	      else
		gfc_error ("Previously initialized symbol '%s' in "
			   "COMMON block '%s' at %C", sym->name, name);
	      goto cleanup;
	    }

	  if (gfc_add_in_common (&sym->attr, sym->name, NULL) == FAILURE)
	    goto cleanup;

	  /* Derived type names must have the SEQUENCE attribute.  */
	  if (sym->ts.type == BT_DERIVED && !sym->ts.derived->attr.sequence)
	    {
	      gfc_error
		("Derived type variable in COMMON at %C does not have the "
		 "SEQUENCE attribute");
	      goto cleanup;
	    }

	  if (tail != NULL)
	    tail->common_next = sym;
	  else
	    *head = sym;

	  tail = sym;

	  /* Deal with an optional array specification after the
             symbol name.  */
	  m = gfc_match_array_spec (&as);
	  if (m == MATCH_ERROR)
	    goto cleanup;

	  if (m == MATCH_YES)
	    {
	      if (as->type != AS_EXPLICIT)
		{
		  gfc_error
		    ("Array specification for symbol '%s' in COMMON at %C "
		     "must be explicit", sym->name);
		  goto cleanup;
		}

	      if (gfc_add_dimension (&sym->attr, sym->name, NULL) == FAILURE)
		goto cleanup;

	      if (sym->attr.pointer)
		{
		  gfc_error
		    ("Symbol '%s' in COMMON at %C cannot be a POINTER array",
		     sym->name);
		  goto cleanup;
		}

	      sym->as = as;
	      as = NULL;
	    }

	  gfc_gobble_whitespace ();
	  if (gfc_match_eos () == MATCH_YES)
	    goto done;
	  if (gfc_peek_char () == '/')
	    break;
	  if (gfc_match_char (',') != MATCH_YES)
	    goto syntax;
	  gfc_gobble_whitespace ();
	  if (gfc_peek_char () == '/')
	    break;
	}
    }

done:
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_COMMON);

cleanup:
  if (old_blank_common)
    old_blank_common->common_next = NULL;
  else
    gfc_current_ns->blank_common.head = NULL;
  gfc_free_array_spec (as);
  return MATCH_ERROR;
}


/* Match a BLOCK DATA program unit.  */

match
gfc_match_block_data (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;

  if (gfc_match_eos () == MATCH_YES)
    {
      gfc_new_block = NULL;
      return MATCH_YES;
    }

  m = gfc_match ("% %n%t", name);
  if (m != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_get_symbol (name, NULL, &sym))
    return MATCH_ERROR;

  if (gfc_add_flavor (&sym->attr, FL_BLOCK_DATA, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Free a namelist structure.  */

void
gfc_free_namelist (gfc_namelist * name)
{
  gfc_namelist *n;

  for (; name; name = n)
    {
      n = name->next;
      gfc_free (name);
    }
}


/* Match a NAMELIST statement.  */

match
gfc_match_namelist (void)
{
  gfc_symbol *group_name, *sym;
  gfc_namelist *nl;
  match m, m2;

  m = gfc_match (" / %s /", &group_name);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto error;

  for (;;)
    {
      if (group_name->ts.type != BT_UNKNOWN)
	{
	  gfc_error
	    ("Namelist group name '%s' at %C already has a basic type "
	     "of %s", group_name->name, gfc_typename (&group_name->ts));
	  return MATCH_ERROR;
	}

      if (group_name->attr.flavor != FL_NAMELIST
	  && gfc_add_flavor (&group_name->attr, FL_NAMELIST,
			     group_name->name, NULL) == FAILURE)
	return MATCH_ERROR;

      for (;;)
	{
	  m = gfc_match_symbol (&sym, 1);
	  if (m == MATCH_NO)
	    goto syntax;
	  if (m == MATCH_ERROR)
	    goto error;

	  if (sym->attr.in_namelist == 0
	      && gfc_add_in_namelist (&sym->attr, sym->name, NULL) == FAILURE)
	    goto error;

	  nl = gfc_get_namelist ();
	  nl->sym = sym;

	  if (group_name->namelist == NULL)
	    group_name->namelist = group_name->namelist_tail = nl;
	  else
	    {
	      group_name->namelist_tail->next = nl;
	      group_name->namelist_tail = nl;
	    }

	  if (gfc_match_eos () == MATCH_YES)
	    goto done;

	  m = gfc_match_char (',');

	  if (gfc_match_char ('/') == MATCH_YES)
	    {
	      m2 = gfc_match (" %s /", &group_name);
	      if (m2 == MATCH_YES)
		break;
	      if (m2 == MATCH_ERROR)
		goto error;
	      goto syntax;
	    }

	  if (m != MATCH_YES)
	    goto syntax;
	}
    }

done:
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_NAMELIST);

error:
  return MATCH_ERROR;
}


/* Match a MODULE statement.  */

match
gfc_match_module (void)
{
  match m;

  m = gfc_match (" %s%t", &gfc_new_block);
  if (m != MATCH_YES)
    return m;

  if (gfc_add_flavor (&gfc_new_block->attr, FL_MODULE,
		      gfc_new_block->name, NULL) == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Free equivalence sets and lists.  Recursively is the easiest way to
   do this.  */

void
gfc_free_equiv (gfc_equiv * eq)
{

  if (eq == NULL)
    return;

  gfc_free_equiv (eq->eq);
  gfc_free_equiv (eq->next);

  gfc_free_expr (eq->expr);
  gfc_free (eq);
}


/* Match an EQUIVALENCE statement.  */

match
gfc_match_equivalence (void)
{
  gfc_equiv *eq, *set, *tail;
  gfc_ref *ref;
  match m;

  tail = NULL;

  for (;;)
    {
      eq = gfc_get_equiv ();
      if (tail == NULL)
	tail = eq;

      eq->next = gfc_current_ns->equiv;
      gfc_current_ns->equiv = eq;

      if (gfc_match_char ('(') != MATCH_YES)
	goto syntax;

      set = eq;

      for (;;)
	{
	  m = gfc_match_variable (&set->expr, 1);
	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    goto syntax;

	  for (ref = set->expr->ref; ref; ref = ref->next)
	    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
	      {
		gfc_error
		  ("Array reference in EQUIVALENCE at %C cannot be an "
		   "array section");
		goto cleanup;
	      }

	  if (gfc_match_char (')') == MATCH_YES)
	    break;
	  if (gfc_match_char (',') != MATCH_YES)
	    goto syntax;

	  set->eq = gfc_get_equiv ();
	  set = set->eq;
	}

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_EQUIVALENCE);

cleanup:
  eq = tail->next;
  tail->next = NULL;

  gfc_free_equiv (gfc_current_ns->equiv);
  gfc_current_ns->equiv = eq;

  return MATCH_ERROR;
}


/* Match a statement function declaration.  It is so easy to match
   non-statement function statements with a MATCH_ERROR as opposed to
   MATCH_NO that we suppress error message in most cases.  */

match
gfc_match_st_function (void)
{
  gfc_error_buf old_error;
  gfc_symbol *sym;
  gfc_expr *expr;
  match m;

  m = gfc_match_symbol (&sym, 0);
  if (m != MATCH_YES)
    return m;

  gfc_push_error (&old_error);

  if (gfc_add_procedure (&sym->attr, PROC_ST_FUNCTION,
			 sym->name, NULL) == FAILURE)
    goto undo_error;

  if (gfc_match_formal_arglist (sym, 1, 0) != MATCH_YES)
    goto undo_error;

  m = gfc_match (" = %e%t", &expr);
  if (m == MATCH_NO)
    goto undo_error;
  if (m == MATCH_ERROR)
    return m;

  sym->value = expr;

  return MATCH_YES;

undo_error:
  gfc_pop_error (&old_error);
  return MATCH_NO;
}


/***************** SELECT CASE subroutines ******************/

/* Free a single case structure.  */

static void
free_case (gfc_case * p)
{
  if (p->low == p->high)
    p->high = NULL;
  gfc_free_expr (p->low);
  gfc_free_expr (p->high);
  gfc_free (p);
}


/* Free a list of case structures.  */

void
gfc_free_case_list (gfc_case * p)
{
  gfc_case *q;

  for (; p; p = q)
    {
      q = p->next;
      free_case (p);
    }
}


/* Match a single case selector.  */

static match
match_case_selector (gfc_case ** cp)
{
  gfc_case *c;
  match m;

  c = gfc_get_case ();
  c->where = gfc_current_locus;

  if (gfc_match_char (':') == MATCH_YES)
    {
      m = gfc_match_init_expr (&c->high);
      if (m == MATCH_NO)
	goto need_expr;
      if (m == MATCH_ERROR)
	goto cleanup;
    }

  else
    {
      m = gfc_match_init_expr (&c->low);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto need_expr;

      /* If we're not looking at a ':' now, make a range out of a single
	 target.  Else get the upper bound for the case range.  */
      if (gfc_match_char (':') != MATCH_YES)
	c->high = c->low;
      else
	{
	  m = gfc_match_init_expr (&c->high);
	  if (m == MATCH_ERROR)
	    goto cleanup;
	  /* MATCH_NO is fine.  It's OK if nothing is there!  */
	}
    }

  *cp = c;
  return MATCH_YES;

need_expr:
  gfc_error ("Expected initialization expression in CASE at %C");

cleanup:
  free_case (c);
  return MATCH_ERROR;
}


/* Match the end of a case statement.  */

static match
match_case_eos (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  match m;

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

  gfc_gobble_whitespace ();

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (strcmp (name, gfc_current_block ()->name) != 0)
    {
      gfc_error ("Expected case name of '%s' at %C",
		 gfc_current_block ()->name);
      return MATCH_ERROR;
    }

  return gfc_match_eos ();
}


/* Match a SELECT statement.  */

match
gfc_match_select (void)
{
  gfc_expr *expr;
  match m;

  m = gfc_match_label ();
  if (m == MATCH_ERROR)
    return m;

  m = gfc_match (" select case ( %e )%t", &expr);
  if (m != MATCH_YES)
    return m;

  new_st.op = EXEC_SELECT;
  new_st.expr = expr;

  return MATCH_YES;
}


/* Match a CASE statement.  */

match
gfc_match_case (void)
{
  gfc_case *c, *head, *tail;
  match m;

  head = tail = NULL;

  if (gfc_current_state () != COMP_SELECT)
    {
      gfc_error ("Unexpected CASE statement at %C");
      return MATCH_ERROR;
    }

  if (gfc_match ("% default") == MATCH_YES)
    {
      m = match_case_eos ();
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      new_st.op = EXEC_SELECT;
      c = gfc_get_case ();
      c->where = gfc_current_locus;
      new_st.ext.case_list = c;
      return MATCH_YES;
    }

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  for (;;)
    {
      if (match_case_selector (&c) == MATCH_ERROR)
	goto cleanup;

      if (head == NULL)
	head = c;
      else
	tail->next = c;

      tail = c;

      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  m = match_case_eos ();
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  new_st.op = EXEC_SELECT;
  new_st.ext.case_list = head;

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in CASE-specification at %C");

cleanup:
  gfc_free_case_list (head);  /* new_st is cleaned up in parse.c.  */
  return MATCH_ERROR;
}

/********************* WHERE subroutines ********************/

/* Match the rest of a simple WHERE statement that follows an IF statement.  
 */

static match
match_simple_where (void)
{
  gfc_expr *expr;
  gfc_code *c;
  match m;

  m = gfc_match (" ( %e )", &expr);
  if (m != MATCH_YES)
    return m;

  m = gfc_match_assignment ();
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_eos () != MATCH_YES)
    goto syntax;

  c = gfc_get_code ();

  c->op = EXEC_WHERE;
  c->expr = expr;
  c->next = gfc_get_code ();

  *c->next = new_st;
  gfc_clear_new_st ();

  new_st.op = EXEC_WHERE;
  new_st.block = c;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_WHERE);

cleanup:
  gfc_free_expr (expr);
  return MATCH_ERROR;
}

/* Match a WHERE statement.  */

match
gfc_match_where (gfc_statement * st)
{
  gfc_expr *expr;
  match m0, m;
  gfc_code *c;

  m0 = gfc_match_label ();
  if (m0 == MATCH_ERROR)
    return m0;

  m = gfc_match (" where ( %e )", &expr);
  if (m != MATCH_YES)
    return m;

  if (gfc_match_eos () == MATCH_YES)
    {
      *st = ST_WHERE_BLOCK;

      new_st.op = EXEC_WHERE;
      new_st.expr = expr;
      return MATCH_YES;
    }

  m = gfc_match_assignment ();
  if (m == MATCH_NO)
    gfc_syntax_error (ST_WHERE);

  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  /* We've got a simple WHERE statement.  */
  *st = ST_WHERE;
  c = gfc_get_code ();

  c->op = EXEC_WHERE;
  c->expr = expr;
  c->next = gfc_get_code ();

  *c->next = new_st;
  gfc_clear_new_st ();

  new_st.op = EXEC_WHERE;
  new_st.block = c;

  return MATCH_YES;
}


/* Match an ELSEWHERE statement.  We leave behind a WHERE node in
   new_st if successful.  */

match
gfc_match_elsewhere (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *expr;
  match m;

  if (gfc_current_state () != COMP_WHERE)
    {
      gfc_error ("ELSEWHERE statement at %C not enclosed in WHERE block");
      return MATCH_ERROR;
    }

  expr = NULL;

  if (gfc_match_char ('(') == MATCH_YES)
    {
      m = gfc_match_expr (&expr);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      if (gfc_match_char (')') != MATCH_YES)
	goto syntax;
    }

  if (gfc_match_eos () != MATCH_YES)
    {				/* Better be a name at this point */
      m = gfc_match_name (name);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      if (gfc_match_eos () != MATCH_YES)
	goto syntax;

      if (strcmp (name, gfc_current_block ()->name) != 0)
	{
	  gfc_error ("Label '%s' at %C doesn't match WHERE label '%s'",
		     name, gfc_current_block ()->name);
	  goto cleanup;
	}
    }

  new_st.op = EXEC_WHERE;
  new_st.expr = expr;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_ELSEWHERE);

cleanup:
  gfc_free_expr (expr);
  return MATCH_ERROR;
}


/******************** FORALL subroutines ********************/

/* Free a list of FORALL iterators.  */

void
gfc_free_forall_iterator (gfc_forall_iterator * iter)
{
  gfc_forall_iterator *next;

  while (iter)
    {
      next = iter->next;

      gfc_free_expr (iter->var);
      gfc_free_expr (iter->start);
      gfc_free_expr (iter->end);
      gfc_free_expr (iter->stride);

      gfc_free (iter);
      iter = next;
    }
}


/* Match an iterator as part of a FORALL statement.  The format is:

     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */

static match
match_forall_iterator (gfc_forall_iterator ** result)
{
  gfc_forall_iterator *iter;
  locus where;
  match m;

  where = gfc_current_locus;
  iter = gfc_getmem (sizeof (gfc_forall_iterator));

  m = gfc_match_variable (&iter->var, 0);
  if (m != MATCH_YES)
    goto cleanup;

  if (gfc_match_char ('=') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  m = gfc_match_expr (&iter->start);
  if (m != MATCH_YES)
    goto cleanup;

  if (gfc_match_char (':') != MATCH_YES)
    goto syntax;

  m = gfc_match_expr (&iter->end);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_char (':') == MATCH_NO)
    iter->stride = gfc_int_expr (1);
  else
    {
      m = gfc_match_expr (&iter->stride);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;
    }

  *result = iter;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in FORALL iterator at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_current_locus = where;
  gfc_free_forall_iterator (iter);
  return m;
}


/* Match the header of a FORALL statement.  */

static match
match_forall_header (gfc_forall_iterator ** phead, gfc_expr ** mask)
{
  gfc_forall_iterator *head, *tail, *new;
  match m;

  gfc_gobble_whitespace ();

  head = tail = NULL;
  *mask = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    return MATCH_NO;

  m = match_forall_iterator (&new);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    goto syntax;

  head = tail = new;

  for (;;)
    {
      if (gfc_match_char (',') != MATCH_YES)
	break;

      m = match_forall_iterator (&new);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  tail->next = new;
	  tail = new;
	  continue;
	}

      /* Have to have a mask expression */

      m = gfc_match_expr (mask);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      break;
    }

  if (gfc_match_char (')') == MATCH_NO)
    goto syntax;

  *phead = head;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_FORALL);

cleanup:
  gfc_free_expr (*mask);
  gfc_free_forall_iterator (head);

  return MATCH_ERROR;
}

/* Match the rest of a simple FORALL statement that follows an IF statement. 
 */

static match
match_simple_forall (void)
{
  gfc_forall_iterator *head;
  gfc_expr *mask;
  gfc_code *c;
  match m;

  mask = NULL;
  head = NULL;
  c = NULL;

  m = match_forall_header (&head, &mask);

  if (m == MATCH_NO)
    goto syntax;
  if (m != MATCH_YES)
    goto cleanup;

  m = gfc_match_assignment ();

  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_pointer_assignment ();
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  c = gfc_get_code ();
  *c = new_st;
  c->loc = gfc_current_locus;

  if (gfc_match_eos () != MATCH_YES)
    goto syntax;

  gfc_clear_new_st ();
  new_st.op = EXEC_FORALL;
  new_st.expr = mask;
  new_st.ext.forall_iterator = head;
  new_st.block = gfc_get_code ();

  new_st.block->op = EXEC_FORALL;
  new_st.block->next = c;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_FORALL);

cleanup:
  gfc_free_forall_iterator (head);
  gfc_free_expr (mask);

  return MATCH_ERROR;
}


/* Match a FORALL statement.  */

match
gfc_match_forall (gfc_statement * st)
{
  gfc_forall_iterator *head;
  gfc_expr *mask;
  gfc_code *c;
  match m0, m;

  head = NULL;
  mask = NULL;
  c = NULL;

  m0 = gfc_match_label ();
  if (m0 == MATCH_ERROR)
    return MATCH_ERROR;

  m = gfc_match (" forall");
  if (m != MATCH_YES)
    return m;

  m = match_forall_header (&head, &mask);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    goto syntax;

  if (gfc_match_eos () == MATCH_YES)
    {
      *st = ST_FORALL_BLOCK;

      new_st.op = EXEC_FORALL;
      new_st.expr = mask;
      new_st.ext.forall_iterator = head;

      return MATCH_YES;
    }

  m = gfc_match_assignment ();
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_pointer_assignment ();
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  c = gfc_get_code ();
  *c = new_st;

  if (gfc_match_eos () != MATCH_YES)
    goto syntax;

  gfc_clear_new_st ();
  new_st.op = EXEC_FORALL;
  new_st.expr = mask;
  new_st.ext.forall_iterator = head;
  new_st.block = gfc_get_code ();

  new_st.block->op = EXEC_FORALL;
  new_st.block->next = c;

  *st = ST_FORALL;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_FORALL);

cleanup:
  gfc_free_forall_iterator (head);
  gfc_free_expr (mask);
  gfc_free_statements (c);
  return MATCH_NO;
}
