/* Keyword tables.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

/* Apply stropping to the given keyword and return its written form, which must
   be upper-case.  */

const char *
a68_strop_keyword (const char *keyword)
{
  char *stropped = (char *) alloca (strlen (keyword) + 1);
  memcpy (stropped, keyword, strlen (keyword) + 1);

  if (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING)
    {
      for (char *p = stropped; *p; ++p)
	*p = TOLOWER (*p);
    }

  return ggc_strdup (stropped);
}

/* Add token to the token tree.  */

TOKEN_T *
a68_add_token (TOKEN_T **p, const char *t)
{
  while (*p != NO_TOKEN)
    {
      int k = strcmp (t, TEXT (*p));

      if (k < 0)
	p = &LESS (*p);
      else if (k > 0)
	p = &MORE (*p);
      else
	return *p;
    }

  *p = (TOKEN_T *) ggc_cleared_alloc<TOKEN_T> ();
  TEXT (*p) = ggc_strdup (t);
  LESS (*p) = MORE (*p) = NO_TOKEN;
  return *p;
}

/*  Find keyword, from token name.  */

KEYWORD_T *
a68_find_keyword (KEYWORD_T *p, const char *t)
{
  while (p != NO_KEYWORD)
    {
      bool case_insensitive = (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING);
      int k = (case_insensitive
	       ? strcasecmp (t, TEXT (p))
	       : strcmp (t, TEXT (p)));

      if (k < 0)
	p = LESS (p);
      else if (k > 0)
	p = MORE (p);
      else
	return p;
    }

  return NO_KEYWORD;
}

/* Find keyword, from attribute.  */

KEYWORD_T *
a68_find_keyword_from_attribute (KEYWORD_T *p, enum a68_attribute a)
{
  if (p == NO_KEYWORD)
    return NO_KEYWORD;
  else if (a == ATTRIBUTE (p))
    return p;
  else
    {
      KEYWORD_T *z;

      if ((z = a68_find_keyword_from_attribute (LESS (p), a)) != NO_KEYWORD)
	return z;
      else if ((z = a68_find_keyword_from_attribute (MORE (p), a)) != NO_KEYWORD)
	return z;
    }

  return NO_KEYWORD;
}

/* Add keyword to the tree.  */

static void
add_keyword (KEYWORD_T **p, enum a68_attribute a, const char *t)
{
  while (*p != NO_KEYWORD)
    {
      bool case_insensitive = (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING);
      int k = (case_insensitive
	       ? strcasecmp (t, TEXT (*p))
	       : strcmp (t, TEXT (*p)));
      if (k < 0)
	p = &LESS (*p);
      else
	p = &MORE (*p);
    }

  *p = (KEYWORD_T *) ggc_cleared_alloc<KEYWORD_T> ();
  ATTRIBUTE (*p) = a;
  TEXT (*p) = t;
  LESS (*p) = MORE (*p) = NO_KEYWORD;
}

/* Make tables of keywords and non-terminals.  */

void
a68_set_up_tables (void)
{
  /* Entries are randomised to balance the tree.  */
  if (OPTION_STRICT (&A68_JOB) == false)
    {
      /* Symbols from GNU extensions.  */
      add_keyword (&A68 (top_keyword), ANDF_SYMBOL, "ANDTH");
      add_keyword (&A68 (top_keyword), ORF_SYMBOL, "OREL");
      add_keyword (&A68 (top_keyword), BRIEF_COMMENT_BEGIN_SYMBOL, "{");
      add_keyword (&A68 (top_keyword), BRIEF_COMMENT_END_SYMBOL, "}");

      if (OPTION_STROPPING (&A68_JOB) != SUPPER_STROPPING)
	{
	  add_keyword (&A68 (top_keyword), BOLD_COMMENT_BEGIN_SYMBOL, "NOTE");
	  add_keyword (&A68 (top_keyword), BOLD_COMMENT_END_SYMBOL, "ETON");
	}

      /* Symbols from MR.  */
      add_keyword (&A68 (top_keyword), ACCESS_SYMBOL, "ACCESS");
      add_keyword (&A68 (top_keyword), DEF_SYMBOL, "DEF");
      add_keyword (&A68 (top_keyword), POSTLUDE_SYMBOL, "POSTLUDE");
      add_keyword (&A68 (top_keyword), FED_SYMBOL, "FED");
      add_keyword (&A68 (top_keyword), FORMAL_NEST_SYMBOL, "NEST");
      add_keyword (&A68 (top_keyword), MODULE_SYMBOL, "MODULE");
      add_keyword (&A68 (top_keyword), EGG_SYMBOL, "EGG");
      add_keyword (&A68 (top_keyword), PUBLIC_SYMBOL, "PUB");
    }

  if (OPTION_STROPPING (&A68_JOB) != SUPPER_STROPPING)
    {
      /* The following representations do not work well with stropping regimes
	 in which reserved words live in the same namespace than
	 tags/identifiers.  The alternative "brief" representations for these
	 symbols shall be used instead.  */
      add_keyword (&A68 (top_keyword), STYLE_I_COMMENT_SYMBOL, "CO");
      add_keyword (&A68 (top_keyword), BOLD_COMMENT_SYMBOL, "COMMENT");
      add_keyword (&A68 (top_keyword), STYLE_II_COMMENT_SYMBOL, "#");
    }

  add_keyword (&A68 (top_keyword), POINT_SYMBOL, ".");
  add_keyword (&A68 (top_keyword), COLON_SYMBOL, ":");
  add_keyword (&A68 (top_keyword), THEN_BAR_SYMBOL, "|");
  add_keyword (&A68 (top_keyword), SUB_SYMBOL, "[");
  add_keyword (&A68 (top_keyword), BY_SYMBOL, "BY");
  add_keyword (&A68 (top_keyword), OP_SYMBOL, "OP");
  add_keyword (&A68 (top_keyword), COMMA_SYMBOL, ",");
  add_keyword (&A68 (top_keyword), AT_SYMBOL, "AT");
  add_keyword (&A68 (top_keyword), PRIO_SYMBOL, "PRIO");
  add_keyword (&A68 (top_keyword), END_SYMBOL, "END");
  add_keyword (&A68 (top_keyword), GO_SYMBOL, "GO");
  add_keyword (&A68 (top_keyword), TO_SYMBOL, "TO");
  add_keyword (&A68 (top_keyword), ELSE_BAR_SYMBOL, "|:");
  add_keyword (&A68 (top_keyword), THEN_SYMBOL, "THEN");
  add_keyword (&A68 (top_keyword), TRUE_SYMBOL, "TRUE");
  add_keyword (&A68 (top_keyword), PROC_SYMBOL, "PROC");
  add_keyword (&A68 (top_keyword), FOR_SYMBOL, "FOR");
  add_keyword (&A68 (top_keyword), GOTO_SYMBOL, "GOTO");
  add_keyword (&A68 (top_keyword), WHILE_SYMBOL, "WHILE");
  add_keyword (&A68 (top_keyword), IS_SYMBOL, ":=:");
  add_keyword (&A68 (top_keyword), ASSIGN_TO_SYMBOL, "=:");
  add_keyword (&A68 (top_keyword), COMPL_SYMBOL, "COMPL");
  add_keyword (&A68 (top_keyword), FROM_SYMBOL, "FROM");
  add_keyword (&A68 (top_keyword), BOLD_PRAGMAT_SYMBOL, "PRAGMAT");
  add_keyword (&A68 (top_keyword), DO_SYMBOL, "DO");
  add_keyword (&A68 (top_keyword), CASE_SYMBOL, "CASE");
  add_keyword (&A68 (top_keyword), LOC_SYMBOL, "LOC");
  add_keyword (&A68 (top_keyword), CHAR_SYMBOL, "CHAR");
  add_keyword (&A68 (top_keyword), ISNT_SYMBOL, ":/=:");
  add_keyword (&A68 (top_keyword), REF_SYMBOL, "REF");
  add_keyword (&A68 (top_keyword), NIL_SYMBOL, "NIL");
  add_keyword (&A68 (top_keyword), ASSIGN_SYMBOL, ":=");
  add_keyword (&A68 (top_keyword), FI_SYMBOL, "FI");
  add_keyword (&A68 (top_keyword), FILE_SYMBOL, "FILE");
  add_keyword (&A68 (top_keyword), PAR_SYMBOL, "PAR");
  add_keyword (&A68 (top_keyword), ASSERT_SYMBOL, "ASSERT");
  add_keyword (&A68 (top_keyword), OUSE_SYMBOL, "OUSE");
  add_keyword (&A68 (top_keyword), IN_SYMBOL, "IN");
  add_keyword (&A68 (top_keyword), LONG_SYMBOL, "LONG");
  add_keyword (&A68 (top_keyword), SEMI_SYMBOL, ";");
  add_keyword (&A68 (top_keyword), EMPTY_SYMBOL, "EMPTY");
  add_keyword (&A68 (top_keyword), MODE_SYMBOL, "MODE");
  add_keyword (&A68 (top_keyword), IF_SYMBOL, "IF");
  add_keyword (&A68 (top_keyword), OD_SYMBOL, "OD");
  add_keyword (&A68 (top_keyword), OF_SYMBOL, "OF");
  add_keyword (&A68 (top_keyword), STRUCT_SYMBOL, "STRUCT");
  add_keyword (&A68 (top_keyword), STYLE_I_PRAGMAT_SYMBOL, "PR");
  add_keyword (&A68 (top_keyword), BUS_SYMBOL, "]");
  add_keyword (&A68 (top_keyword), SKIP_SYMBOL, "SKIP");
  add_keyword (&A68 (top_keyword), SHORT_SYMBOL, "SHORT");
  add_keyword (&A68 (top_keyword), IS_SYMBOL, "IS");
  add_keyword (&A68 (top_keyword), ESAC_SYMBOL, "ESAC");
  add_keyword (&A68 (top_keyword), CHANNEL_SYMBOL, "CHANNEL");
  add_keyword (&A68 (top_keyword), REAL_SYMBOL, "REAL");
  add_keyword (&A68 (top_keyword), STRING_SYMBOL, "STRING");
  add_keyword (&A68 (top_keyword), BOOL_SYMBOL, "BOOL");
  add_keyword (&A68 (top_keyword), ISNT_SYMBOL, "ISNT");
  add_keyword (&A68 (top_keyword), FALSE_SYMBOL, "FALSE");
  add_keyword (&A68 (top_keyword), UNION_SYMBOL, "UNION");
  add_keyword (&A68 (top_keyword), OUT_SYMBOL, "OUT");
  add_keyword (&A68 (top_keyword), BRIEF_COMMENT_END_SYMBOL, "{");
  add_keyword (&A68 (top_keyword), OPEN_SYMBOL, "(");
  add_keyword (&A68 (top_keyword), BEGIN_SYMBOL, "BEGIN");
  add_keyword (&A68 (top_keyword), FLEX_SYMBOL, "FLEX");
  add_keyword (&A68 (top_keyword), VOID_SYMBOL, "VOID");
  add_keyword (&A68 (top_keyword), BITS_SYMBOL, "BITS");
  add_keyword (&A68 (top_keyword), ELSE_SYMBOL, "ELSE");
  add_keyword (&A68 (top_keyword), EXIT_SYMBOL, "EXIT");
  add_keyword (&A68 (top_keyword), HEAP_SYMBOL, "HEAP");
  add_keyword (&A68 (top_keyword), INT_SYMBOL, "INT");
  add_keyword (&A68 (top_keyword), BYTES_SYMBOL, "BYTES");
  add_keyword (&A68 (top_keyword), SEMA_SYMBOL, "SEMA");
  add_keyword (&A68 (top_keyword), CLOSE_SYMBOL, ")");
  add_keyword (&A68 (top_keyword), AT_SYMBOL, "@");
  add_keyword (&A68 (top_keyword), ELIF_SYMBOL, "ELIF");
}
