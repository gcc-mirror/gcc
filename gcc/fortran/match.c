/* Matching subroutines in all sizes, shapes and colors.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "flags.h"
#include "gfortran.h"
#include "match.h"
#include "parse.h"

int gfc_matching_procptr_assignment = 0;
bool gfc_matching_prefix = false;

/* Stack of SELECT TYPE statements.  */
gfc_select_type_stack *select_type_stack = NULL;

/* For debugging and diagnostic purposes.  Return the textual representation
   of the intrinsic operator OP.  */
const char *
gfc_op2string (gfc_intrinsic_op op)
{
  switch (op)
    {
    case INTRINSIC_UPLUS:
    case INTRINSIC_PLUS:
      return "+";

    case INTRINSIC_UMINUS:
    case INTRINSIC_MINUS:
      return "-";

    case INTRINSIC_POWER:
      return "**";
    case INTRINSIC_CONCAT:
      return "//";
    case INTRINSIC_TIMES:
      return "*";
    case INTRINSIC_DIVIDE:
      return "/";

    case INTRINSIC_AND:
      return ".and.";
    case INTRINSIC_OR:
      return ".or.";
    case INTRINSIC_EQV:
      return ".eqv.";
    case INTRINSIC_NEQV:
      return ".neqv.";

    case INTRINSIC_EQ_OS:
      return ".eq.";
    case INTRINSIC_EQ:
      return "==";
    case INTRINSIC_NE_OS:
      return ".ne.";
    case INTRINSIC_NE:
      return "/=";
    case INTRINSIC_GE_OS:
      return ".ge.";
    case INTRINSIC_GE:
      return ">=";
    case INTRINSIC_LE_OS:
      return ".le.";
    case INTRINSIC_LE:
      return "<=";
    case INTRINSIC_LT_OS:
      return ".lt.";
    case INTRINSIC_LT:
      return "<";
    case INTRINSIC_GT_OS:
      return ".gt.";
    case INTRINSIC_GT:
      return ">";
    case INTRINSIC_NOT:
      return ".not.";

    case INTRINSIC_ASSIGN:
      return "=";

    case INTRINSIC_PARENTHESES:
      return "parens";

    default:
      break;
    }

  gfc_internal_error ("gfc_op2string(): Bad code");
  /* Not reached.  */
}


/******************** Generic matching subroutines ************************/

/* This function scans the current statement counting the opened and closed
   parenthesis to make sure they are balanced.  */

match
gfc_match_parens (void)
{
  locus old_loc, where;
  int count, instring;
  gfc_char_t c, quote;

  old_loc = gfc_current_locus;
  count = 0;
  instring = 0;
  quote = ' ';

  for (;;)
    {
      c = gfc_next_char_literal (instring);
      if (c == '\n')
	break;
      if (quote == ' ' && ((c == '\'') || (c == '"')))
	{
	  quote = c;
	  instring = 1;
	  continue;
	}
      if (quote != ' ' && c == quote)
	{
	  quote = ' ';
	  instring = 0;
	  continue;
	}

      if (c == '(' && quote == ' ')
	{
	  count++;
	  where = gfc_current_locus;
	}
      if (c == ')' && quote == ' ')
	{
	  count--;
	  where = gfc_current_locus;
	}
    }

  gfc_current_locus = old_loc;

  if (count > 0)
    {
      gfc_error ("Missing ')' in statement at or before %L", &where);
      return MATCH_ERROR;
    }
  if (count < 0)
    {
      gfc_error ("Missing '(' in statement at or before %L", &where);
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* See if the next character is a special character that has
   escaped by a \ via the -fbackslash option.  */

match
gfc_match_special_char (gfc_char_t *res)
{
  int len, i;
  gfc_char_t c, n;
  match m;

  m = MATCH_YES;

  switch ((c = gfc_next_char_literal (1)))
    {
    case 'a':
      *res = '\a';
      break;
    case 'b':
      *res = '\b';
      break;
    case 't':
      *res = '\t';
      break;
    case 'f':
      *res = '\f';
      break;
    case 'n':
      *res = '\n';
      break;
    case 'r':
      *res = '\r';
      break;
    case 'v':
      *res = '\v';
      break;
    case '\\':
      *res = '\\';
      break;
    case '0':
      *res = '\0';
      break;

    case 'x':
    case 'u':
    case 'U':
      /* Hexadecimal form of wide characters.  */
      len = (c == 'x' ? 2 : (c == 'u' ? 4 : 8));
      n = 0;
      for (i = 0; i < len; i++)
	{
	  char buf[2] = { '\0', '\0' };

	  c = gfc_next_char_literal (1);
	  if (!gfc_wide_fits_in_byte (c)
	      || !gfc_check_digit ((unsigned char) c, 16))
	    return MATCH_NO;

	  buf[0] = (unsigned char) c;
	  n = n << 4;
	  n += strtol (buf, NULL, 16);
	}
      *res = n;
      break;

    default:
      /* Unknown backslash codes are simply not expanded.  */
      m = MATCH_NO;
      break;
    }

  return m;
}


/* In free form, match at least one space.  Always matches in fixed
   form.  */

match
gfc_match_space (void)
{
  locus old_loc;
  char c;

  if (gfc_current_form == FORM_FIXED)
    return MATCH_YES;

  old_loc = gfc_current_locus;

  c = gfc_next_ascii_char ();
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
  int flag;
  char c;

  flag = 0;

  for (;;)
    {
      old_loc = gfc_current_locus;
      gfc_gobble_whitespace ();

      c = gfc_next_ascii_char ();
      switch (c)
	{
	case '!':
	  do
	    {
	      c = gfc_next_ascii_char ();
	    }
	  while (c != '\n');

	  /* Fall through.  */

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
   old-style character length specifications.  If cnt is non-NULL it
   will be set to the number of digits.  */

match
gfc_match_small_literal_int (int *value, int *cnt)
{
  locus old_loc;
  char c;
  int i, j;

  old_loc = gfc_current_locus;

  *value = -1;
  gfc_gobble_whitespace ();
  c = gfc_next_ascii_char ();
  if (cnt)
    *cnt = 0;

  if (!ISDIGIT (c))
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  i = c - '0';
  j = 1;

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_ascii_char ();

      if (!ISDIGIT (c))
	break;

      i = 10 * i + c - '0';
      j++;

      if (i > 99999999)
	{
	  gfc_error ("Integer too large at %C");
	  return MATCH_ERROR;
	}
    }

  gfc_current_locus = old_loc;

  *value = i;
  if (cnt)
    *cnt = j;
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


/* This function is the same as the gfc_match_small_int, except that
   we're keeping the pointer to the expr.  This function could just be
   removed and the previously mentioned one modified, though all calls
   to it would have to be modified then (and there were a number of
   them).  Return MATCH_ERROR if fail to extract the int; otherwise,
   return the result of gfc_match_expr().  The expr (if any) that was
   matched is returned in the parameter expr.  */

match
gfc_match_small_int_expr (int *value, gfc_expr **expr)
{
  const char *p;
  match m;
  int i;

  m = gfc_match_expr (expr);
  if (m != MATCH_YES)
    return m;

  p = gfc_extract_int (*expr, &i);

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
gfc_match_st_label (gfc_st_label **label)
{
  locus old_loc;
  match m;
  int i, cnt;

  old_loc = gfc_current_locus;

  m = gfc_match_small_literal_int (&i, &cnt);
  if (m != MATCH_YES)
    return m;

  if (cnt > 5)
    {
      gfc_error ("Too many digits in statement label at %C");
      goto cleanup;
    }

  if (i == 0)
    {
      gfc_error ("Statement label at %C is zero");
      goto cleanup;
    }

  *label = gfc_get_st_label (i);
  return MATCH_YES;

cleanup:

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

  if (gfc_new_block->attr.flavor == FL_LABEL)
    {
      gfc_error ("Duplicate construct label '%s' at %C", name);
      return MATCH_ERROR;
    }

  if (gfc_add_flavor (&gfc_new_block->attr, FL_LABEL,
		      gfc_new_block->name, NULL) == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* See if the current input looks like a name of some sort.  Modifies
   the passed buffer which must be GFC_MAX_SYMBOL_LEN+1 bytes long.
   Note that options.c restricts max_identifier_length to not more
   than GFC_MAX_SYMBOL_LEN.  */

match
gfc_match_name (char *buffer)
{
  locus old_loc;
  int i;
  char c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  c = gfc_next_ascii_char ();
  if (!(ISALPHA (c) || (c == '_' && gfc_option.flag_allow_leading_underscore)))
    {
      if (gfc_error_flag_test() == 0 && c != '(')
	gfc_error ("Invalid character in name at %C");
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
      c = gfc_next_ascii_char ();
    }
  while (ISALNUM (c) || c == '_' || (gfc_option.flag_dollar_ok && c == '$'));

  if (c == '$' && !gfc_option.flag_dollar_ok)
    {
      gfc_error ("Invalid character '$' at %C. Use -fdollar-ok to allow it "
		 "as an extension");
      return MATCH_ERROR;
    }

  buffer[i] = '\0';
  gfc_current_locus = old_loc;

  return MATCH_YES;
}


/* Match a valid name for C, which is almost the same as for Fortran,
   except that you can start with an underscore, etc..  It could have
   been done by modifying the gfc_match_name, but this way other
   things C allows can be added, such as no limits on the length.
   Right now, the length is limited to the same thing as Fortran..
   Also, by rewriting it, we use the gfc_next_char_C() to prevent the
   input characters from being automatically lower cased, since C is
   case sensitive.  The parameter, buffer, is used to return the name
   that is matched.  Return MATCH_ERROR if the name is too long
   (though this is a self-imposed limit), MATCH_NO if what we're
   seeing isn't a name, and MATCH_YES if we successfully match a C
   name.  */

match
gfc_match_name_C (char *buffer)
{
  locus old_loc;
  int i = 0;
  gfc_char_t c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  /* Get the next char (first possible char of name) and see if
     it's valid for C (either a letter or an underscore).  */
  c = gfc_next_char_literal (1);

  /* If the user put nothing expect spaces between the quotes, it is valid
     and simply means there is no name= specifier and the name is the fortran
     symbol name, all lowercase.  */
  if (c == '"' || c == '\'')
    {
      buffer[0] = '\0';
      gfc_current_locus = old_loc;
      return MATCH_YES;
    }
  
  if (!ISALPHA (c) && c != '_')
    {
      gfc_error ("Invalid C name in NAME= specifier at %C");
      return MATCH_ERROR;
    }

  /* Continue to read valid variable name characters.  */
  do
    {
      gcc_assert (gfc_wide_fits_in_byte (c));

      buffer[i++] = (unsigned char) c;
      
    /* C does not define a maximum length of variable names, to my
       knowledge, but the compiler typically places a limit on them.
       For now, i'll use the same as the fortran limit for simplicity,
       but this may need to be changed to a dynamic buffer that can
       be realloc'ed here if necessary, or more likely, a larger
       upper-bound set.  */
      if (i > gfc_option.max_identifier_length)
        {
          gfc_error ("Name at %C is too long");
          return MATCH_ERROR;
        }
      
      old_loc = gfc_current_locus;
      
      /* Get next char; param means we're in a string.  */
      c = gfc_next_char_literal (1);
    } while (ISALNUM (c) || c == '_');

  buffer[i] = '\0';
  gfc_current_locus = old_loc;

  /* See if we stopped because of whitespace.  */
  if (c == ' ')
    {
      gfc_gobble_whitespace ();
      c = gfc_peek_ascii_char ();
      if (c != '"' && c != '\'')
        {
          gfc_error ("Embedded space in NAME= specifier at %C");
          return MATCH_ERROR;
        }
    }
  
  /* If we stopped because we had an invalid character for a C name, report
     that to the user by returning MATCH_NO.  */
  if (c != '"' && c != '\'')
    {
      gfc_error ("Invalid C name in NAME= specifier at %C");
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Match a symbol on the input.  Modifies the pointer to the symbol
   pointer if successful.  */

match
gfc_match_sym_tree (gfc_symtree **matched_symbol, int host_assoc)
{
  char buffer[GFC_MAX_SYMBOL_LEN + 1];
  match m;

  m = gfc_match_name (buffer);
  if (m != MATCH_YES)
    return m;

  if (host_assoc)
    return (gfc_get_ha_sym_tree (buffer, matched_symbol))
	    ? MATCH_ERROR : MATCH_YES;

  if (gfc_get_sym_tree (buffer, NULL, matched_symbol, false))
    return MATCH_ERROR;

  return MATCH_YES;
}


match
gfc_match_symbol (gfc_symbol **matched_symbol, int host_assoc)
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
  else
    *matched_symbol = NULL;
  return m;
}


/* Match an intrinsic operator.  Returns an INTRINSIC enum. While matching, 
   we always find INTRINSIC_PLUS before INTRINSIC_UPLUS. We work around this 
   in matchexp.c.  */

match
gfc_match_intrinsic_op (gfc_intrinsic_op *result)
{
  locus orig_loc = gfc_current_locus;
  char ch;

  gfc_gobble_whitespace ();
  ch = gfc_next_ascii_char ();
  switch (ch)
    {
    case '+':
      /* Matched "+".  */
      *result = INTRINSIC_PLUS;
      return MATCH_YES;

    case '-':
      /* Matched "-".  */
      *result = INTRINSIC_MINUS;
      return MATCH_YES;

    case '=':
      if (gfc_next_ascii_char () == '=')
	{
	  /* Matched "==".  */
	  *result = INTRINSIC_EQ;
	  return MATCH_YES;
	}
      break;

    case '<':
      if (gfc_peek_ascii_char () == '=')
	{
	  /* Matched "<=".  */
	  gfc_next_ascii_char ();
	  *result = INTRINSIC_LE;
	  return MATCH_YES;
	}
      /* Matched "<".  */
      *result = INTRINSIC_LT;
      return MATCH_YES;

    case '>':
      if (gfc_peek_ascii_char () == '=')
	{
	  /* Matched ">=".  */
	  gfc_next_ascii_char ();
	  *result = INTRINSIC_GE;
	  return MATCH_YES;
	}
      /* Matched ">".  */
      *result = INTRINSIC_GT;
      return MATCH_YES;

    case '*':
      if (gfc_peek_ascii_char () == '*')
	{
	  /* Matched "**".  */
	  gfc_next_ascii_char ();
	  *result = INTRINSIC_POWER;
	  return MATCH_YES;
	}
      /* Matched "*".  */
      *result = INTRINSIC_TIMES;
      return MATCH_YES;

    case '/':
      ch = gfc_peek_ascii_char ();
      if (ch == '=')
	{
	  /* Matched "/=".  */
	  gfc_next_ascii_char ();
	  *result = INTRINSIC_NE;
	  return MATCH_YES;
	}
      else if (ch == '/')
	{
	  /* Matched "//".  */
	  gfc_next_ascii_char ();
	  *result = INTRINSIC_CONCAT;
	  return MATCH_YES;
	}
      /* Matched "/".  */
      *result = INTRINSIC_DIVIDE;
      return MATCH_YES;

    case '.':
      ch = gfc_next_ascii_char ();
      switch (ch)
	{
	case 'a':
	  if (gfc_next_ascii_char () == 'n'
	      && gfc_next_ascii_char () == 'd'
	      && gfc_next_ascii_char () == '.')
	    {
	      /* Matched ".and.".  */
	      *result = INTRINSIC_AND;
	      return MATCH_YES;
	    }
	  break;

	case 'e':
	  if (gfc_next_ascii_char () == 'q')
	    {
	      ch = gfc_next_ascii_char ();
	      if (ch == '.')
		{
		  /* Matched ".eq.".  */
		  *result = INTRINSIC_EQ_OS;
		  return MATCH_YES;
		}
	      else if (ch == 'v')
		{
		  if (gfc_next_ascii_char () == '.')
		    {
		      /* Matched ".eqv.".  */
		      *result = INTRINSIC_EQV;
		      return MATCH_YES;
		    }
		}
	    }
	  break;

	case 'g':
	  ch = gfc_next_ascii_char ();
	  if (ch == 'e')
	    {
	      if (gfc_next_ascii_char () == '.')
		{
		  /* Matched ".ge.".  */
		  *result = INTRINSIC_GE_OS;
		  return MATCH_YES;
		}
	    }
	  else if (ch == 't')
	    {
	      if (gfc_next_ascii_char () == '.')
		{
		  /* Matched ".gt.".  */
		  *result = INTRINSIC_GT_OS;
		  return MATCH_YES;
		}
	    }
	  break;

	case 'l':
	  ch = gfc_next_ascii_char ();
	  if (ch == 'e')
	    {
	      if (gfc_next_ascii_char () == '.')
		{
		  /* Matched ".le.".  */
		  *result = INTRINSIC_LE_OS;
		  return MATCH_YES;
		}
	    }
	  else if (ch == 't')
	    {
	      if (gfc_next_ascii_char () == '.')
		{
		  /* Matched ".lt.".  */
		  *result = INTRINSIC_LT_OS;
		  return MATCH_YES;
		}
	    }
	  break;

	case 'n':
	  ch = gfc_next_ascii_char ();
	  if (ch == 'e')
	    {
	      ch = gfc_next_ascii_char ();
	      if (ch == '.')
		{
		  /* Matched ".ne.".  */
		  *result = INTRINSIC_NE_OS;
		  return MATCH_YES;
		}
	      else if (ch == 'q')
		{
		  if (gfc_next_ascii_char () == 'v'
		      && gfc_next_ascii_char () == '.')
		    {
		      /* Matched ".neqv.".  */
		      *result = INTRINSIC_NEQV;
		      return MATCH_YES;
		    }
		}
	    }
	  else if (ch == 'o')
	    {
	      if (gfc_next_ascii_char () == 't'
		  && gfc_next_ascii_char () == '.')
		{
		  /* Matched ".not.".  */
		  *result = INTRINSIC_NOT;
		  return MATCH_YES;
		}
	    }
	  break;

	case 'o':
	  if (gfc_next_ascii_char () == 'r'
	      && gfc_next_ascii_char () == '.')
	    {
	      /* Matched ".or.".  */
	      *result = INTRINSIC_OR;
	      return MATCH_YES;
	    }
	  break;

	default:
	  break;
	}
      break;

    default:
      break;
    }

  gfc_current_locus = orig_loc;
  return MATCH_NO;
}


/* Match a loop control phrase:

    <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]

   If the final integer expression is not present, a constant unity
   expression is returned.  We don't return MATCH_ERROR until after
   the equals sign is seen.  */

match
gfc_match_iterator (gfc_iterator *iter, int init_flag)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *var, *e1, *e2, *e3;
  locus start;
  match m;

  /* Match the start of an iterator without affecting the symbol table.  */

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

  var->symtree->n.sym->attr.implied_index = 1;

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

  if (gfc_next_ascii_char () == c)
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
	  n = gfc_match_st_label (label);
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
	  break;	/* Fall through to character matcher.  */

	default:
	  gfc_internal_error ("gfc_match(): Bad match code %c", c);
	}

    default:

      /* gfc_next_ascii_char converts characters to lower-case, so we shouldn't
	 expect an upper case character here!  */
      gcc_assert (TOLOWER (c) == c);

      if (c == gfc_next_ascii_char ())
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
	      break;		/* Skip.  */

	    /* Matches that don't have to be undone */
	    case 'o':
	    case 'l':
	    case 'n':
	    case 's':
	      (void) va_arg (argp, void **);
	      break;

	    case 'e':
	    case 'v':
	      vp = va_arg (argp, void **);
	      gfc_free_expr ((struct gfc_expr *)*vp);
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

  lvalue = NULL;
  m = gfc_match (" %v =", &lvalue);
  if (m != MATCH_YES)
    {
      gfc_current_locus = old_loc;
      gfc_free_expr (lvalue);
      return MATCH_NO;
    }

  rvalue = NULL;
  m = gfc_match (" %e%t", &rvalue);
  if (m != MATCH_YES)
    {
      gfc_current_locus = old_loc;
      gfc_free_expr (lvalue);
      gfc_free_expr (rvalue);
      return m;
    }

  gfc_set_sym_referenced (lvalue->symtree->n.sym);

  new_st.op = EXEC_ASSIGN;
  new_st.expr1 = lvalue;
  new_st.expr2 = rvalue;

  gfc_check_do_variable (lvalue->symtree);

  return MATCH_YES;
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
  gfc_matching_procptr_assignment = 0;

  m = gfc_match (" %v =>", &lvalue);
  if (m != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  if (lvalue->symtree->n.sym->attr.proc_pointer
      || gfc_is_proc_ptr_comp (lvalue, NULL))
    gfc_matching_procptr_assignment = 1;

  m = gfc_match (" %e%t", &rvalue);
  gfc_matching_procptr_assignment = 0;
  if (m != MATCH_YES)
    goto cleanup;

  new_st.op = EXEC_POINTER_ASSIGN;
  new_st.expr1 = lvalue;
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

  if (gfc_notify_std (GFC_STD_F95_OBS, "Obsolescent feature: Arithmetic IF "
		      "statement at %C") == FAILURE)
    return MATCH_ERROR;

  new_st.op = EXEC_ARITHMETIC_IF;
  new_st.expr1 = expr;
  new_st.label1 = l1;
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
gfc_match_if (gfc_statement *if_type)
{
  gfc_expr *expr;
  gfc_st_label *l1, *l2, *l3;
  locus old_loc, old_loc2;
  gfc_code *p;
  match m, n;

  n = gfc_match_label ();
  if (n == MATCH_ERROR)
    return n;

  old_loc = gfc_current_locus;

  m = gfc_match (" if ( %e", &expr);
  if (m != MATCH_YES)
    return m;

  old_loc2 = gfc_current_locus;
  gfc_current_locus = old_loc;
  
  if (gfc_match_parens () == MATCH_ERROR)
    return MATCH_ERROR;

  gfc_current_locus = old_loc2;

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
	  gfc_error ("Block label not appropriate for arithmetic IF "
		     "statement at %C");
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
      
      if (gfc_notify_std (GFC_STD_F95_OBS, "Obsolescent feature: Arithmetic IF "
			  "statement at %C") == FAILURE)
	return MATCH_ERROR;

      new_st.op = EXEC_ARITHMETIC_IF;
      new_st.expr1 = expr;
      new_st.label1 = l1;
      new_st.label2 = l2;
      new_st.label3 = l3;

      *if_type = ST_ARITHMETIC_IF;
      return MATCH_YES;
    }

  if (gfc_match (" then%t") == MATCH_YES)
    {
      new_st.op = EXEC_IF;
      new_st.expr1 = expr;
      *if_type = ST_IF_BLOCK;
      return MATCH_YES;
    }

  if (n == MATCH_YES)
    {
      gfc_error ("Block label is not appropriate for IF statement at %C");
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

  /* m can be MATCH_NO or MATCH_ERROR, here.  For MATCH_ERROR, a mangled
     assignment was found.  For MATCH_NO, continue to call the various
     matchers.  */
  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  gfc_match (" if ( %e ) ", &expr);	/* Guaranteed to match.  */

  m = gfc_match_pointer_assignment ();
  if (m == MATCH_YES)
    goto got_match;

  gfc_free_expr (expr);
  gfc_undo_symbols ();
  gfc_current_locus = old_loc;

  gfc_match (" if ( %e ) ", &expr);	/* Guaranteed to match.  */

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
  match ("flush", gfc_match_flush, ST_FLUSH)
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
  match ("wait", gfc_match_wait, ST_WAIT)
  match ("where", match_simple_where, ST_WHERE)
  match ("write", gfc_match_write, ST_WRITE)

  /* The gfc_match_assignment() above may have returned a MATCH_NO
     where the assignment was to a named constant.  Check that 
     special case here.  */
  m = gfc_match_assignment ();
  if (m == MATCH_NO)
   {
      gfc_error ("Cannot assign to a named constant at %C");
      gfc_free_expr (expr);
      gfc_undo_symbols ();
      gfc_current_locus = old_loc;
      return MATCH_ERROR;
   }

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

  p->expr1 = expr;
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
  new_st.expr1 = expr;
  return MATCH_YES;

cleanup:
  gfc_free_expr (expr);
  return MATCH_ERROR;
}


/* Free a gfc_iterator structure.  */

void
gfc_free_iterator (gfc_iterator *iter, int flag)
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


/* Match a BLOCK statement.  */

match
gfc_match_block (void)
{
  match m;

  if (gfc_match_label () == MATCH_ERROR)
    return MATCH_ERROR;

  if (gfc_match (" block") != MATCH_YES)
    return MATCH_NO;

  /* For this to be a correct BLOCK statement, the line must end now.  */
  m = gfc_match_eos ();
  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (m == MATCH_NO)
    return MATCH_NO;

  return MATCH_YES;
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

  m = gfc_match_st_label (&label);
  if (m == MATCH_ERROR)
    goto cleanup;

  /* Match an infinite DO, make it like a DO WHILE(.TRUE.).  */

  if (gfc_match_eos () == MATCH_YES)
    {
      iter.end = gfc_logical_expr (1, NULL);
      new_st.op = EXEC_DO_WHILE;
      goto done;
    }

  /* Match an optional comma, if no comma is found, a space is obligatory.  */
  if (gfc_match_char (',') != MATCH_YES && gfc_match ("% ") != MATCH_YES)
    return MATCH_NO;

  /* Check for balanced parens.  */
  
  if (gfc_match_parens () == MATCH_ERROR)
    return MATCH_ERROR;

  /* See if we have a DO WHILE.  */
  if (gfc_match (" while ( %e )%t", &iter.end) == MATCH_YES)
    {
      new_st.op = EXEC_DO_WHILE;
      goto done;
    }

  /* The abortive DO WHILE may have done something to the symbol
     table, so we start over.  */
  gfc_undo_symbols ();
  gfc_current_locus = old_loc;

  gfc_match_label ();		/* This won't error.  */
  gfc_match (" do ");		/* This will work.  */

  gfc_match_st_label (&label);	/* Can't error out.  */
  gfc_match_char (',');		/* Optional comma.  */

  m = gfc_match_iterator (&iter, 0);
  if (m == MATCH_NO)
    return MATCH_NO;
  if (m == MATCH_ERROR)
    goto cleanup;

  iter.var->symtree->n.sym->attr.implied_index = 0;
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

  new_st.label1 = label;

  if (new_st.op == EXEC_DO_WHILE)
    new_st.expr1 = iter.end;
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
  gfc_state_data *p, *o;
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

  /* Find the loop mentioned specified by the label (or lack of a label).  */
  for (o = NULL, p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_DO && (sym == NULL || sym == p->sym))
      break;
    else if (o == NULL && p->state == COMP_OMP_STRUCTURED_BLOCK)
      o = p;

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

  if (o != NULL)
    {
      gfc_error ("%s statement at %C leaving OpenMP structured block",
		 gfc_ascii_statement (st));
      return MATCH_ERROR;
    }
  else if (st == ST_EXIT
	   && p->previous != NULL
	   && p->previous->state == COMP_OMP_STRUCTURED_BLOCK
	   && (p->previous->head->op == EXEC_OMP_DO
	       || p->previous->head->op == EXEC_OMP_PARALLEL_DO))
    {
      gcc_assert (p->previous->head->next != NULL);
      gcc_assert (p->previous->head->next->op == EXEC_DO
		  || p->previous->head->next->op == EXEC_DO_WHILE);
      gfc_error ("EXIT statement at %C terminating !$OMP DO loop");
      return MATCH_ERROR;
    }

  /* Save the first statement in the loop - needed by the backend.  */
  new_st.ext.whichloop = p->head;

  new_st.op = op;

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
  int cnt;

  stop_code = -1;
  e = NULL;

  if (gfc_match_eos () != MATCH_YES)
    {
      m = gfc_match_small_literal_int (&stop_code, &cnt);
      if (m == MATCH_ERROR)
	goto cleanup;

      if (m == MATCH_YES && cnt > 5)
	{
	  gfc_error ("Too many digits in STOP code at %C");
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
  new_st.expr1 = e;
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
      if (gfc_notify_std (GFC_STD_F95_DEL, "Deleted feature: PAUSE statement"
	  " at %C")
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
	  if (gfc_notify_std (GFC_STD_F95_DEL, "Deleted feature: ASSIGN "
			      "statement at %C")
	      == FAILURE)
	    return MATCH_ERROR;

	  expr->symtree->n.sym->attr.assign = 1;

	  new_st.op = EXEC_LABEL_ASSIGN;
	  new_st.label1 = label;
	  new_st.expr1 = expr;
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
      new_st.label1 = label;
      return MATCH_YES;
    }

  /* The assigned GO TO statement.  */ 

  if (gfc_match_variable (&expr, 0) == MATCH_YES)
    {
      if (gfc_notify_std (GFC_STD_F95_DEL, "Deleted feature: Assigned GOTO "
			  "statement at %C")
	  == FAILURE)
	return MATCH_ERROR;

      new_st.op = EXEC_GOTO;
      new_st.expr1 = expr;

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
	  m = gfc_match_st_label (&label);
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

	  tail->label1 = label;
	  tail->op = EXEC_GOTO;
	}
      while (gfc_match_char (',') == MATCH_YES);

      if (gfc_match (")%t") != MATCH_YES)
	goto syntax;

      if (head == NULL)
	{
	   gfc_error ("Statement label list in GOTO at %C cannot be empty");
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
      m = gfc_match_st_label (&label);
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
      tail->next->label1 = label;
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

  if (gfc_notify_std (GFC_STD_F95_OBS, "Obsolescent feature: Computed GOTO "
		      "at %C") == FAILURE)
    return MATCH_ERROR;

  /* At this point, a computed GOTO has been fully matched and an
     equivalent SELECT statement constructed.  */

  new_st.op = EXEC_SELECT;
  new_st.expr1 = NULL;

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
gfc_free_alloc_list (gfc_alloc *p)
{
  gfc_alloc *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free_expr (p->expr);
      gfc_free (p);
    }
}


/* Match a Fortran 2003 derived-type-spec (F03:R455), which is just the name of
   an accessible derived type.  */

static match
match_derived_type_spec (gfc_typespec *ts)
{
  locus old_locus; 
  gfc_symbol *derived;

  old_locus = gfc_current_locus; 

  if (gfc_match_symbol (&derived, 1) == MATCH_YES)
    {
      if (derived->attr.flavor == FL_DERIVED)
	{
	  ts->type = BT_DERIVED;
	  ts->u.derived = derived;
	  return MATCH_YES;
	}
      else
	{
	  /* Enforce F03:C476.  */
	  gfc_error ("'%s' at %L is not an accessible derived type",
		     derived->name, &gfc_current_locus);
	  return MATCH_ERROR;
	}
    }

  gfc_current_locus = old_locus; 
  return MATCH_NO;
}


/* Match a Fortran 2003 type-spec (F03:R401).  This is similar to
   gfc_match_decl_type_spec() from decl.c, with the following exceptions:
   It only includes the intrinsic types from the Fortran 2003 standard
   (thus, neither BYTE nor forms like REAL*4 are allowed). Additionally,
   the implicit_flag is not needed, so it was removed.  Derived types are
   identified by their name alone.  */

static match
match_type_spec (gfc_typespec *ts)
{
  match m;
  locus old_locus;

  gfc_clear_ts (ts);
  old_locus = gfc_current_locus;

  if (gfc_match ("integer") == MATCH_YES)
    {
      ts->type = BT_INTEGER;
      ts->kind = gfc_default_integer_kind;
      goto kind_selector;
    }

  if (gfc_match ("real") == MATCH_YES)
    {
      ts->type = BT_REAL;
      ts->kind = gfc_default_real_kind;
      goto kind_selector;
    }

  if (gfc_match ("double precision") == MATCH_YES)
    {
      ts->type = BT_REAL;
      ts->kind = gfc_default_double_kind;
      return MATCH_YES;
    }

  if (gfc_match ("complex") == MATCH_YES)
    {
      ts->type = BT_COMPLEX;
      ts->kind = gfc_default_complex_kind;
      goto kind_selector;
    }

  if (gfc_match ("character") == MATCH_YES)
    {
      ts->type = BT_CHARACTER;
      goto char_selector;
    }

  if (gfc_match ("logical") == MATCH_YES)
    {
      ts->type = BT_LOGICAL;
      ts->kind = gfc_default_logical_kind;
      goto kind_selector;
    }

  m = match_derived_type_spec (ts);
  if (m == MATCH_YES)
    {
      old_locus = gfc_current_locus;
      if (gfc_match (" :: ") != MATCH_YES)
	return MATCH_ERROR;
      gfc_current_locus = old_locus;
      /* Enfore F03:C401.  */
      if (ts->u.derived->attr.abstract)
	{
	  gfc_error ("Derived type '%s' at %L may not be ABSTRACT",
		     ts->u.derived->name, &old_locus);
	  return MATCH_ERROR;
	}
      return MATCH_YES;
    }
  else if (m == MATCH_ERROR && gfc_match (" :: ") == MATCH_YES)
    return MATCH_ERROR;

  /* If a type is not matched, simply return MATCH_NO.  */
  gfc_current_locus = old_locus;
  return MATCH_NO;

kind_selector:

  gfc_gobble_whitespace ();
  if (gfc_peek_ascii_char () == '*')
    {
      gfc_error ("Invalid type-spec at %C");
      return MATCH_ERROR;
    }

  m = gfc_match_kind_spec (ts, false);

  if (m == MATCH_NO)
    m = MATCH_YES;		/* No kind specifier found.  */

  return m;

char_selector:

  m = gfc_match_char_spec (ts);

  if (m == MATCH_NO)
    m = MATCH_YES;		/* No kind specifier found.  */

  return m;
}


/* Match an ALLOCATE statement.  */

match
gfc_match_allocate (void)
{
  gfc_alloc *head, *tail;
  gfc_expr *stat, *errmsg, *tmp, *source;
  gfc_typespec ts;
  gfc_symbol *sym;
  match m;
  locus old_locus;
  bool saw_stat, saw_errmsg, saw_source, b1, b2, b3;

  head = tail = NULL;
  stat = errmsg = source = tmp = NULL;
  saw_stat = saw_errmsg = saw_source = false;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  /* Match an optional type-spec.  */
  old_locus = gfc_current_locus;
  m = match_type_spec (&ts);
  if (m == MATCH_ERROR)
    goto cleanup;
  else if (m == MATCH_NO)
    ts.type = BT_UNKNOWN;
  else
    {
      if (gfc_match (" :: ") == MATCH_YES)
	{
	  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: typespec in "
			      "ALLOCATE at %L", &old_locus) == FAILURE)
	    goto cleanup;
	}
      else
	{
	  ts.type = BT_UNKNOWN;
	  gfc_current_locus = old_locus;
	}
    }

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

      if (gfc_pure (NULL) && gfc_impure_variable (tail->expr->symtree->n.sym))
	{
	  gfc_error ("Bad allocate-object at %C for a PURE procedure");
	  goto cleanup;
	}

      /* The ALLOCATE statement had an optional typespec.  Check the
	 constraints.  */
      if (ts.type != BT_UNKNOWN)
	{
	  /* Enforce F03:C624.  */
	  if (!gfc_type_compatible (&tail->expr->ts, &ts))
	    {
	      gfc_error ("Type of entity at %L is type incompatible with "
			 "typespec", &tail->expr->where);
	      goto cleanup;
	    }

	  /* Enforce F03:C627.  */
	  if (ts.kind != tail->expr->ts.kind)
	    {
	      gfc_error ("Kind type parameter for entity at %L differs from "
			 "the kind type parameter of the typespec",
			 &tail->expr->where);
	      goto cleanup;
	    }
	}

      if (tail->expr->ts.type == BT_DERIVED)
	tail->expr->ts.u.derived = gfc_use_derived (tail->expr->ts.u.derived);

      /* FIXME: disable the checking on derived types and arrays.  */
      sym = tail->expr->symtree->n.sym;
      b1 = !(tail->expr->ref
	   && (tail->expr->ref->type == REF_COMPONENT
		|| tail->expr->ref->type == REF_ARRAY));
      if (sym && sym->ts.type == BT_CLASS)
	b2 = !(sym->ts.u.derived->components->attr.allocatable
	       || sym->ts.u.derived->components->attr.pointer);
      else
	b2 = sym && !(sym->attr.allocatable || sym->attr.pointer
		      || sym->attr.proc_pointer);
      b3 = sym && sym->ns && sym->ns->proc_name
	   && (sym->ns->proc_name->attr.allocatable
		|| sym->ns->proc_name->attr.pointer
		|| sym->ns->proc_name->attr.proc_pointer);
      if (b1 && b2 && !b3)
	{
	  gfc_error ("Allocate-object at %C is not a nonprocedure pointer "
		     "or an allocatable variable");
	  goto cleanup;
	}

      if (gfc_match_char (',') != MATCH_YES)
	break;

alloc_opt_list:

      m = gfc_match (" stat = %v", &tmp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  /* Enforce C630.  */
	  if (saw_stat)
	    {
	      gfc_error ("Redundant STAT tag found at %L ", &tmp->where);
	      goto cleanup;
	    }

	  stat = tmp;
	  saw_stat = true;

	  if (gfc_check_do_variable (stat->symtree))
	    goto cleanup;

	  if (gfc_match_char (',') == MATCH_YES)
	    goto alloc_opt_list;
	}

      m = gfc_match (" errmsg = %v", &tmp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: ERRMSG tag at %L",
			      &tmp->where) == FAILURE)
	    goto cleanup;

	  /* Enforce C630.  */
	  if (saw_errmsg)
	    {
	      gfc_error ("Redundant ERRMSG tag found at %L ", &tmp->where);
	      goto cleanup;
	    }

	  errmsg = tmp;
	  saw_errmsg = true;

	  if (gfc_match_char (',') == MATCH_YES)
	    goto alloc_opt_list;
	}

      m = gfc_match (" source = %e", &tmp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: SOURCE tag at %L",
			      &tmp->where) == FAILURE)
	    goto cleanup;

	  /* Enforce C630.  */
	  if (saw_source)
	    {
	      gfc_error ("Redundant SOURCE tag found at %L ", &tmp->where);
	      goto cleanup;
	    }

	  /* The next 2 conditionals check C631.  */
	  if (ts.type != BT_UNKNOWN)
	    {
	      gfc_error ("SOURCE tag at %L conflicts with the typespec at %L",
			 &tmp->where, &old_locus);
	      goto cleanup;
	    }

	  if (head->next)
	    {
 	      gfc_error ("SOURCE tag at %L requires only a single entity in "
			 "the allocation-list", &tmp->where);
	      goto cleanup;
            }

	  source = tmp;
	  saw_source = true;

	  if (gfc_match_char (',') == MATCH_YES)
	    goto alloc_opt_list;
	}

	gfc_gobble_whitespace ();

	if (gfc_peek_char () == ')')
	  break;
    }


  if (gfc_match (" )%t") != MATCH_YES)
    goto syntax;

  new_st.op = EXEC_ALLOCATE;
  new_st.expr1 = stat;
  new_st.expr2 = errmsg;
  new_st.expr3 = source;
  new_st.ext.alloc.list = head;
  new_st.ext.alloc.ts = ts;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_ALLOCATE);

cleanup:
  gfc_free_expr (errmsg);
  gfc_free_expr (source);
  gfc_free_expr (stat);
  gfc_free_expr (tmp);
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

      if (gfc_check_do_variable (p->symtree))
	goto cleanup;

      if (gfc_pure (NULL) && gfc_impure_variable (p->symtree->n.sym))
	{
	  gfc_error ("Illegal variable in NULLIFY at %C for a PURE procedure");
	  goto cleanup;
	}

      /* build ' => NULL() '.  */
      e = gfc_get_expr ();
      e->where = gfc_current_locus;
      e->expr_type = EXPR_NULL;
      e->ts.type = BT_UNKNOWN;

      /* Chain to list.  */
      if (tail == NULL)
	tail = &new_st;
      else
	{
	  tail->next = gfc_get_code ();
	  tail = tail->next;
	}

      tail->op = EXEC_POINTER_ASSIGN;
      tail->expr1 = p;
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
  gfc_free_statements (new_st.next);
  new_st.next = NULL;
  gfc_free_expr (new_st.expr1);
  new_st.expr1 = NULL;
  gfc_free_expr (new_st.expr2);
  new_st.expr2 = NULL;
  return MATCH_ERROR;
}


/* Match a DEALLOCATE statement.  */

match
gfc_match_deallocate (void)
{
  gfc_alloc *head, *tail;
  gfc_expr *stat, *errmsg, *tmp;
  gfc_symbol *sym;
  match m;
  bool saw_stat, saw_errmsg, b1, b2;

  head = tail = NULL;
  stat = errmsg = tmp = NULL;
  saw_stat = saw_errmsg = false;

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

      sym = tail->expr->symtree->n.sym;

      if (gfc_pure (NULL) && gfc_impure_variable (sym))
	{
	  gfc_error ("Illegal allocate-object at %C for a PURE procedure");
	  goto cleanup;
	}

      /* FIXME: disable the checking on derived types.  */
      b1 = !(tail->expr->ref
	   && (tail->expr->ref->type == REF_COMPONENT
	       || tail->expr->ref->type == REF_ARRAY));
      if (sym && sym->ts.type == BT_CLASS)
	b2 = !(sym->ts.u.derived->components->attr.allocatable
	       || sym->ts.u.derived->components->attr.pointer);
      else
	b2 = sym && !(sym->attr.allocatable || sym->attr.pointer
		      || sym->attr.proc_pointer);
      if (b1 && b2)
	{
	  gfc_error ("Allocate-object at %C is not a nonprocedure pointer "
		     "or an allocatable variable");
	  goto cleanup;
	}

      if (gfc_match_char (',') != MATCH_YES)
	break;

dealloc_opt_list:

      m = gfc_match (" stat = %v", &tmp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  if (saw_stat)
	    {
	      gfc_error ("Redundant STAT tag found at %L ", &tmp->where);
	      gfc_free_expr (tmp);
	      goto cleanup;
	    }

	  stat = tmp;
	  saw_stat = true;

	  if (gfc_check_do_variable (stat->symtree))
	    goto cleanup;

	  if (gfc_match_char (',') == MATCH_YES)
	    goto dealloc_opt_list;
	}

      m = gfc_match (" errmsg = %v", &tmp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: ERRMSG at %L",
			      &tmp->where) == FAILURE)
	    goto cleanup;

	  if (saw_errmsg)
	    {
	      gfc_error ("Redundant ERRMSG tag found at %L ", &tmp->where);
	      gfc_free_expr (tmp);
	      goto cleanup;
	    }

	  errmsg = tmp;
	  saw_errmsg = true;

	  if (gfc_match_char (',') == MATCH_YES)
	    goto dealloc_opt_list;
	}

	gfc_gobble_whitespace ();

	if (gfc_peek_char () == ')')
	  break;
    }

  if (gfc_match (" )%t") != MATCH_YES)
    goto syntax;

  new_st.op = EXEC_DEALLOCATE;
  new_st.expr1 = stat;
  new_st.expr2 = errmsg;
  new_st.ext.alloc.list = head;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DEALLOCATE);

cleanup:
  gfc_free_expr (errmsg);
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

  e = NULL;
  if (gfc_match_eos () == MATCH_YES)
    goto done;

  if (gfc_find_state (COMP_SUBROUTINE) == FAILURE)
    {
      gfc_error ("Alternate RETURN statement at %C is only allowed within "
		 "a SUBROUTINE");
      goto cleanup;
    }

  if (gfc_notify_std (GFC_STD_F95_OBS, "Obsolescent feature: Alternate RETURN "
		      "at %C") == FAILURE)
    return MATCH_ERROR;

  if (gfc_current_form == FORM_FREE)
    {
      /* The following are valid, so we can't require a blank after the
	RETURN keyword:
	  return+1
	  return(1)  */
      char c = gfc_peek_ascii_char ();
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
  new_st.expr1 = e;

  return MATCH_YES;
}


/* Match the call of a type-bound procedure, if CALL%var has already been 
   matched and var found to be a derived-type variable.  */

static match
match_typebound_call (gfc_symtree* varst)
{
  gfc_expr* base;
  match m;

  base = gfc_get_expr ();
  base->expr_type = EXPR_VARIABLE;
  base->symtree = varst;
  base->where = gfc_current_locus;
  gfc_set_sym_referenced (varst->n.sym);
  
  m = gfc_match_varspec (base, 0, true, true);
  if (m == MATCH_NO)
    gfc_error ("Expected component reference at %C");
  if (m != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Junk after CALL at %C");
      return MATCH_ERROR;
    }

  if (base->expr_type == EXPR_COMPCALL)
    new_st.op = EXEC_COMPCALL;
  else if (base->expr_type == EXPR_PPC)
    new_st.op = EXEC_CALL_PPC;
  else
    {
      gfc_error ("Expected type-bound procedure or procedure pointer component "
		 "at %C");
      return MATCH_ERROR;
    }
  new_st.expr1 = base;

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

  /* If this is a variable of derived-type, it probably starts a type-bound
     procedure call.  */
  if ((sym->attr.flavor != FL_PROCEDURE
       || gfc_is_function_return_value (sym, gfc_current_ns))
      && (sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS))
    return match_typebound_call (st);

  /* If it does not seem to be callable (include functions so that the
     right association is made.  They are thrown out in resolution.)
     ...  */
  if (!sym->attr.generic
	&& !sym->attr.subroutine
	&& !sym->attr.function)
    {
      if (!(sym->attr.external && !sym->attr.referenced))
	{
	  /* ...create a symbol in this scope...  */
	  if (sym->ns != gfc_current_ns
	        && gfc_get_sym_tree (name, NULL, &st, false) == 1)
            return MATCH_ERROR;

	  if (sym != st->n.sym)
	    sym = st->n.sym;
	}

      /* ...and then to try to make the symbol into a subroutine.  */
      if (gfc_add_subroutine (&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;
    }

  gfc_set_sym_referenced (sym);

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
      sprintf (name, "_result_%s", sym->name);
      gfc_get_ha_sym_tree (name, &select_st);   /* Can't fail.  */

      select_sym = select_st->n.sym;
      select_sym->ts.type = BT_INTEGER;
      select_sym->ts.kind = gfc_default_integer_kind;
      gfc_set_sym_referenced (select_sym);
      c->expr1 = gfc_get_expr ();
      c->expr1->expr_type = EXPR_VARIABLE;
      c->expr1->symtree = select_st;
      c->expr1->ts = select_sym->ts;
      c->expr1->where = gfc_current_locus;

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
	  c->next->label1 = a->label;
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
  char mangled_name[GFC_MAX_SYMBOL_LEN + 1];

  if (from_module)
    {
      /* A use associated common block is only needed to correctly layout
	 the variables it contains.  */
      snprintf (mangled_name, GFC_MAX_SYMBOL_LEN, "_%d_%s", serial++, name);
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

match match_common_name (char *name)
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
  gfc_symbol *sym, **head, *tail, *other, *old_blank_common;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_common_head *t;
  gfc_array_spec *as;
  gfc_equiv *e1, *e2;
  match m;
  gfc_gsymbol *gsym;

  old_blank_common = gfc_current_ns->blank_common.head;
  if (old_blank_common)
    {
      while (old_blank_common->common_next)
	old_blank_common = old_blank_common->common_next;
    }

  as = NULL;

  for (;;)
    {
      m = match_common_name (name);
      if (m == MATCH_ERROR)
	goto cleanup;

      gsym = gfc_get_gsymbol (name);
      if (gsym->type != GSYM_UNKNOWN && gsym->type != GSYM_COMMON)
	{
	  gfc_error ("Symbol '%s' at %C is already an external symbol that "
		     "is not COMMON", name);
	  goto cleanup;
	}

      if (gsym->type == GSYM_UNKNOWN)
	{
	  gsym->type = GSYM_COMMON;
	  gsym->where = gfc_current_locus;
	  gsym->defined = 1;
	}

      gsym->used = 1;

      if (name[0] == '\0')
	{
	  t = &gfc_current_ns->blank_common;
	  if (t->head == NULL)
	    t->where = gfc_current_locus;
	}
      else
	{
	  t = gfc_get_common (name, 0);
	}
      head = &t->head;

      if (*head == NULL)
	tail = NULL;
      else
	{
	  tail = *head;
	  while (tail->common_next)
	    tail = tail->common_next;
	}

      /* Grab the list of symbols.  */
      for (;;)
	{
	  m = gfc_match_symbol (&sym, 0);
	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    goto syntax;

          /* Store a ref to the common block for error checking.  */
          sym->common_block = t;
          
          /* See if we know the current common block is bind(c), and if
             so, then see if we can check if the symbol is (which it'll
             need to be).  This can happen if the bind(c) attr stmt was
             applied to the common block, and the variable(s) already
             defined, before declaring the common block.  */
          if (t->is_bind_c == 1)
            {
              if (sym->ts.type != BT_UNKNOWN && sym->ts.is_c_interop != 1)
                {
                  /* If we find an error, just print it and continue,
                     cause it's just semantic, and we can see if there
                     are more errors.  */
                  gfc_error_now ("Variable '%s' at %L in common block '%s' "
                                 "at %C must be declared with a C "
                                 "interoperable kind since common block "
                                 "'%s' is bind(c)",
                                 sym->name, &(sym->declared_at), t->name,
                                 t->name);
                }
              
              if (sym->attr.is_bind_c == 1)
                gfc_error_now ("Variable '%s' in common block "
                               "'%s' at %C can not be bind(c) since "
                               "it is not global", sym->name, t->name);
            }
          
	  if (sym->attr.in_common)
	    {
	      gfc_error ("Symbol '%s' at %C is already in a COMMON block",
			 sym->name);
	      goto cleanup;
	    }

	  if (((sym->value != NULL && sym->value->expr_type != EXPR_NULL)
	       || sym->attr.data) && gfc_current_state () != COMP_BLOCK_DATA)
	    {
	      if (gfc_notify_std (GFC_STD_GNU, "Initialized symbol '%s' at %C "
					       "can only be COMMON in "
					       "BLOCK DATA", sym->name)
		  == FAILURE)
		goto cleanup;
	    }

	  if (gfc_add_in_common (&sym->attr, sym->name, NULL) == FAILURE)
	    goto cleanup;

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
		  gfc_error ("Array specification for symbol '%s' in COMMON "
			     "at %C must be explicit", sym->name);
		  goto cleanup;
		}

	      if (gfc_add_dimension (&sym->attr, sym->name, NULL) == FAILURE)
		goto cleanup;

	      if (sym->attr.pointer)
		{
		  gfc_error ("Symbol '%s' in COMMON at %C cannot be a "
			     "POINTER array", sym->name);
		  goto cleanup;
		}

	      sym->as = as;
	      as = NULL;

	    }

	  sym->common_head = t;

	  /* Check to see if the symbol is already in an equivalence group.
	     If it is, set the other members as being in common.  */
	  if (sym->attr.in_equivalence)
	    {
	      for (e1 = gfc_current_ns->equiv; e1; e1 = e1->next)
		{
		  for (e2 = e1; e2; e2 = e2->eq)
		    if (e2->expr->symtree->n.sym == sym)
		      goto equiv_found;

		  continue;

	  equiv_found:

		  for (e2 = e1; e2; e2 = e2->eq)
		    {
		      other = e2->expr->symtree->n.sym;
		      if (other->common_head
			  && other->common_head != sym->common_head)
			{
			  gfc_error ("Symbol '%s', in COMMON block '%s' at "
				     "%C is being indirectly equivalenced to "
				     "another COMMON block '%s'",
				     sym->name, sym->common_head->name,
				     other->common_head->name);
			    goto cleanup;
			}
		      other->attr.in_common = 1;
		      other->common_head = t;
		    }
		}
	    }


	  gfc_gobble_whitespace ();
	  if (gfc_match_eos () == MATCH_YES)
	    goto done;
	  if (gfc_peek_ascii_char () == '/')
	    break;
	  if (gfc_match_char (',') != MATCH_YES)
	    goto syntax;
	  gfc_gobble_whitespace ();
	  if (gfc_peek_ascii_char () == '/')
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
gfc_free_namelist (gfc_namelist *name)
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
	  gfc_error ("Namelist group name '%s' at %C already has a basic "
		     "type of %s", group_name->name,
		     gfc_typename (&group_name->ts));
	  return MATCH_ERROR;
	}

      if (group_name->attr.flavor == FL_NAMELIST
	  && group_name->attr.use_assoc
	  && gfc_notify_std (GFC_STD_GNU, "Namelist group name '%s' "
			     "at %C already is USE associated and can"
			     "not be respecified.", group_name->name)
	     == FAILURE)
	return MATCH_ERROR;

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

	  /* Use gfc_error_check here, rather than goto error, so that
	     these are the only errors for the next two lines.  */
	  if (sym->as && sym->as->type == AS_ASSUMED_SIZE)
	    {
	      gfc_error ("Assumed size array '%s' in namelist '%s' at "
			 "%C is not allowed", sym->name, group_name->name);
	      gfc_error_check ();
	    }

	  if (sym->ts.type == BT_CHARACTER && sym->ts.u.cl->length == NULL)
	    {
	      gfc_error ("Assumed character length '%s' in namelist '%s' at "
			 "%C is not allowed", sym->name, group_name->name);
	      gfc_error_check ();
	    }

	  nl = gfc_get_namelist ();
	  nl->sym = sym;
	  sym->refs++;

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
gfc_free_equiv (gfc_equiv *eq)
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
  gfc_symbol *sym;
  match m;
  gfc_common_head *common_head = NULL;
  bool common_flag;
  int cnt;

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
      common_flag = FALSE;
      cnt = 0;

      for (;;)
	{
	  m = gfc_match_equiv_variable (&set->expr);
	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    goto syntax;

	  /*  count the number of objects.  */
	  cnt++;

	  if (gfc_match_char ('%') == MATCH_YES)
	    {
	      gfc_error ("Derived type component %C is not a "
			 "permitted EQUIVALENCE member");
	      goto cleanup;
	    }

	  for (ref = set->expr->ref; ref; ref = ref->next)
	    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
	      {
		gfc_error ("Array reference in EQUIVALENCE at %C cannot "
			   "be an array section");
		goto cleanup;
	      }

	  sym = set->expr->symtree->n.sym;

	  if (gfc_add_in_equivalence (&sym->attr, sym->name, NULL) == FAILURE)
	    goto cleanup;

	  if (sym->attr.in_common)
	    {
	      common_flag = TRUE;
	      common_head = sym->common_head;
	    }

	  if (gfc_match_char (')') == MATCH_YES)
	    break;

	  if (gfc_match_char (',') != MATCH_YES)
	    goto syntax;

	  set->eq = gfc_get_equiv ();
	  set = set->eq;
	}

      if (cnt < 2)
	{
	  gfc_error ("EQUIVALENCE at %C requires two or more objects");
	  goto cleanup;
	}

      /* If one of the members of an equivalence is in common, then
	 mark them all as being in common.  Before doing this, check
	 that members of the equivalence group are not in different
	 common blocks.  */
      if (common_flag)
	for (set = eq; set; set = set->eq)
	  {
	    sym = set->expr->symtree->n.sym;
	    if (sym->common_head && sym->common_head != common_head)
	      {
		gfc_error ("Attempt to indirectly overlap COMMON "
			   "blocks %s and %s by EQUIVALENCE at %C",
			   sym->common_head->name, common_head->name);
		goto cleanup;
	      }
	    sym->attr.in_common = 1;
	    sym->common_head = common_head;
	  }

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expecting a comma in EQUIVALENCE at %C");
	  goto cleanup;
	}
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


/* Check that a statement function is not recursive. This is done by looking
   for the statement function symbol(sym) by looking recursively through its
   expression(e).  If a reference to sym is found, true is returned.  
   12.5.4 requires that any variable of function that is implicitly typed
   shall have that type confirmed by any subsequent type declaration.  The
   implicit typing is conveniently done here.  */
static bool
recursive_stmt_fcn (gfc_expr *, gfc_symbol *);

static bool
check_stmt_fcn (gfc_expr *e, gfc_symbol *sym, int *f ATTRIBUTE_UNUSED)
{

  if (e == NULL)
    return false;

  switch (e->expr_type)
    {
    case EXPR_FUNCTION:
      if (e->symtree == NULL)
	return false;

      /* Check the name before testing for nested recursion!  */
      if (sym->name == e->symtree->n.sym->name)
	return true;

      /* Catch recursion via other statement functions.  */
      if (e->symtree->n.sym->attr.proc == PROC_ST_FUNCTION
	  && e->symtree->n.sym->value
	  && recursive_stmt_fcn (e->symtree->n.sym->value, sym))
	return true;

      if (e->symtree->n.sym->ts.type == BT_UNKNOWN)
	gfc_set_default_type (e->symtree->n.sym, 0, NULL);

      break;

    case EXPR_VARIABLE:
      if (e->symtree && sym->name == e->symtree->n.sym->name)
	return true;

      if (e->symtree->n.sym->ts.type == BT_UNKNOWN)
	gfc_set_default_type (e->symtree->n.sym, 0, NULL);
      break;

    default:
      break;
    }

  return false;
}


static bool
recursive_stmt_fcn (gfc_expr *e, gfc_symbol *sym)
{
  return gfc_traverse_expr (e, sym, check_stmt_fcn, 0);
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

  gfc_free_error (&old_error);
  if (m == MATCH_ERROR)
    return m;

  if (recursive_stmt_fcn (expr, sym))
    {
      gfc_error ("Statement function at %L is recursive", &expr->where);
      return MATCH_ERROR;
    }

  sym->value = expr;

  if (gfc_notify_std (GFC_STD_F95_OBS, "Obsolescent feature: "
		      "Statement function at %C") == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;

undo_error:
  gfc_pop_error (&old_error);
  return MATCH_NO;
}


/***************** SELECT CASE subroutines ******************/

/* Free a single case structure.  */

static void
free_case (gfc_case *p)
{
  if (p->low == p->high)
    p->high = NULL;
  gfc_free_expr (p->low);
  gfc_free_expr (p->high);
  gfc_free (p);
}


/* Free a list of case structures.  */

void
gfc_free_case_list (gfc_case *p)
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
match_case_selector (gfc_case **cp)
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

  /* If the case construct doesn't have a case-construct-name, we
     should have matched the EOS.  */
  if (!gfc_current_block ())
    return MATCH_NO;

  gfc_gobble_whitespace ();

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (strcmp (name, gfc_current_block ()->name) != 0)
    {
      gfc_error ("Expected block name '%s' of SELECT construct at %C",
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
  new_st.expr1 = expr;

  return MATCH_YES;
}


/* Push the current selector onto the SELECT TYPE stack.  */

static void
select_type_push (gfc_symbol *sel)
{
  gfc_select_type_stack *top = gfc_get_select_type_stack ();
  top->selector = sel;
  top->tmp = NULL;
  top->prev = select_type_stack;

  select_type_stack = top;
}


/* Set the temporary for the current SELECT TYPE selector.  */

static void
select_type_set_tmp (gfc_typespec *ts)
{
  char name[GFC_MAX_SYMBOL_LEN];
  gfc_symtree *tmp;

  sprintf (name, "tmp$%s", ts->u.derived->name);
  gfc_get_sym_tree (name, gfc_current_ns, &tmp, false);
  gfc_add_type (tmp->n.sym, ts, NULL);
  gfc_set_sym_referenced (tmp->n.sym);
  gfc_add_pointer (&tmp->n.sym->attr, NULL);
  gfc_add_flavor (&tmp->n.sym->attr, FL_VARIABLE, name, NULL);

  select_type_stack->tmp = tmp;
}


/* Match a SELECT TYPE statement.  */

match
gfc_match_select_type (void)
{
  gfc_expr *expr1, *expr2 = NULL;
  match m;
  char name[GFC_MAX_SYMBOL_LEN];

  m = gfc_match_label ();
  if (m == MATCH_ERROR)
    return m;

  m = gfc_match (" select type ( ");
  if (m != MATCH_YES)
    return m;

  gfc_current_ns = gfc_build_block_ns (gfc_current_ns);

  m = gfc_match (" %n => %e", name, &expr2);
  if (m == MATCH_YES)
    {
      expr1 = gfc_get_expr();
      expr1->expr_type = EXPR_VARIABLE;
      if (gfc_get_sym_tree (name, NULL, &expr1->symtree, false))
	return MATCH_ERROR;
      expr1->symtree->n.sym->ts = expr2->ts;
      expr1->symtree->n.sym->attr.referenced = 1;
      expr1->symtree->n.sym->attr.class_ok = 1;
    }
  else
    {
      m = gfc_match (" %e ", &expr1);
      if (m != MATCH_YES)
	return m;
    }

  m = gfc_match (" )%t");
  if (m != MATCH_YES)
    return m;

  /* Check for F03:C811.  */
  if (!expr2 && (expr1->expr_type != EXPR_VARIABLE || expr1->ref != NULL))
    {
      gfc_error ("Selector in SELECT TYPE at %C is not a named variable; "
		 "use associate-name=>");
      return MATCH_ERROR;
    }

  /* Check for F03:C813.  */
  if (expr1->ts.type != BT_CLASS && !(expr2 && expr2->ts.type == BT_CLASS))
    {
      gfc_error ("Selector shall be polymorphic in SELECT TYPE statement "
		 "at %C");
      return MATCH_ERROR;
    }

  new_st.op = EXEC_SELECT_TYPE;
  new_st.expr1 = expr1;
  new_st.expr2 = expr2;
  new_st.ext.ns = gfc_current_ns;

  select_type_push (expr1->symtree->n.sym);

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
  gfc_error ("Syntax error in CASE specification at %C");

cleanup:
  gfc_free_case_list (head);  /* new_st is cleaned up in parse.c.  */
  return MATCH_ERROR;
}


/* Match a TYPE IS statement.  */

match
gfc_match_type_is (void)
{
  gfc_case *c = NULL;
  match m;

  if (gfc_current_state () != COMP_SELECT_TYPE)
    {
      gfc_error ("Unexpected TYPE IS statement at %C");
      return MATCH_ERROR;
    }

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  c = gfc_get_case ();
  c->where = gfc_current_locus;

  /* TODO: Once unlimited polymorphism is implemented, we will need to call
     match_type_spec here.  */
  if (match_derived_type_spec (&c->ts) == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;

  m = match_case_eos ();
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  new_st.op = EXEC_SELECT_TYPE;
  new_st.ext.case_list = c;

  /* Create temporary variable.  */
  select_type_set_tmp (&c->ts);

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in TYPE IS specification at %C");

cleanup:
  if (c != NULL)
    gfc_free_case_list (c);  /* new_st is cleaned up in parse.c.  */
  return MATCH_ERROR;
}


/* Match a CLASS IS or CLASS DEFAULT statement.  */

match
gfc_match_class_is (void)
{
  gfc_case *c = NULL;
  match m;

  if (gfc_current_state () != COMP_SELECT_TYPE)
    return MATCH_NO;

  if (gfc_match ("% default") == MATCH_YES)
    {
      m = match_case_eos ();
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      new_st.op = EXEC_SELECT_TYPE;
      c = gfc_get_case ();
      c->where = gfc_current_locus;
      c->ts.type = BT_UNKNOWN;
      new_st.ext.case_list = c;
      return MATCH_YES;
    }

  m = gfc_match ("% is");
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  c = gfc_get_case ();
  c->where = gfc_current_locus;

  if (match_derived_type_spec (&c->ts) == MATCH_ERROR)
    goto cleanup;

  if (c->ts.type == BT_DERIVED)
    c->ts.type = BT_CLASS;

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;

  m = match_case_eos ();
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  new_st.op = EXEC_SELECT_TYPE;
  new_st.ext.case_list = c;

  gfc_error_now ("CLASS IS specification at %C is not yet supported");

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in CLASS IS specification at %C");

cleanup:
  if (c != NULL)
    gfc_free_case_list (c);  /* new_st is cleaned up in parse.c.  */
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
  c->expr1 = expr;
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
gfc_match_where (gfc_statement *st)
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
      new_st.expr1 = expr;
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
  c->expr1 = expr;
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
    {
      /* Only makes sense if we have a where-construct-name.  */
      if (!gfc_current_block ())
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      /* Better be a name at this point.  */
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
  new_st.expr1 = expr;
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
gfc_free_forall_iterator (gfc_forall_iterator *iter)
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

     <var> = <start>:<end>[:<stride>]

   On MATCH_NO, the caller tests for the possibility that there is a
   scalar mask expression.  */

static match
match_forall_iterator (gfc_forall_iterator **result)
{
  gfc_forall_iterator *iter;
  locus where;
  match m;

  where = gfc_current_locus;
  iter = XCNEW (gfc_forall_iterator);

  m = gfc_match_expr (&iter->var);
  if (m != MATCH_YES)
    goto cleanup;

  if (gfc_match_char ('=') != MATCH_YES
      || iter->var->expr_type != EXPR_VARIABLE)
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

  /* Mark the iteration variable's symbol as used as a FORALL index.  */
  iter->var->symtree->n.sym->forall_index = true;

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
match_forall_header (gfc_forall_iterator **phead, gfc_expr **mask)
{
  gfc_forall_iterator *head, *tail, *new_iter;
  gfc_expr *msk;
  match m;

  gfc_gobble_whitespace ();

  head = tail = NULL;
  msk = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    return MATCH_NO;

  m = match_forall_iterator (&new_iter);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    goto syntax;

  head = tail = new_iter;

  for (;;)
    {
      if (gfc_match_char (',') != MATCH_YES)
	break;

      m = match_forall_iterator (&new_iter);
      if (m == MATCH_ERROR)
	goto cleanup;

      if (m == MATCH_YES)
	{
	  tail->next = new_iter;
	  tail = new_iter;
	  continue;
	}

      /* Have to have a mask expression.  */

      m = gfc_match_expr (&msk);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      break;
    }

  if (gfc_match_char (')') == MATCH_NO)
    goto syntax;

  *phead = head;
  *mask = msk;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_FORALL);

cleanup:
  gfc_free_expr (msk);
  gfc_free_forall_iterator (head);

  return MATCH_ERROR;
}

/* Match the rest of a simple FORALL statement that follows an 
   IF statement.  */

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
  new_st.expr1 = mask;
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
gfc_match_forall (gfc_statement *st)
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
      new_st.expr1 = mask;
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
  c->loc = gfc_current_locus;

  gfc_clear_new_st ();
  new_st.op = EXEC_FORALL;
  new_st.expr1 = mask;
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
