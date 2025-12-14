/* Handling of compiler pragmats.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

/* Utility macros useful for parsing pragmat contents.  */

#define SKIP_WHITESPACES(P)			\
  while (ISSPACE (*(P))) (P)++

#define PARSE_WORD(P,W)					\
  do							\
    {							\
      (W) = (char *) alloca (strlen ((P)));		\
      size_t i = 0;					\
      while (ISALPHA (*(P)))				\
	(W)[i++] = *((P)++);				\
      (W)[i] = '\0';					\
    } while (0)

/* Parse a string denotation and return its value in an allocated buffer in
   *STR.  It is up to the caller to dispose of the allocated memory.

   This function returns a pointer to the character following the string
   denotation in P, or P in case of parse error.

   In case of parse error *STR is also set to NULL.  */

const char *
parse_string_denotation (const char *p, char **str)
{
  const char *orig = p;
  size_t i = 0;
  char *s = (char *) xmalloc (2 * strlen (p));

  if (*(p++) != '\"')
    goto error;
  while (*p != '\0' && *p != '\"')
    {
      if (*p == '\'' && *(p + 1) == '\"')
	{
	  s[i++] = '\"';
	  p += 2;
	}
      else
	{
	  s[i++] = *p;
	  p++;
	}
    }
  if (*p != '\"')
    goto error;

  *str = s;
  return p + 1;
 error:
  free (s);
  *str = NULL;
  return orig;
}

/* PR access MODULE in "filename" PR  */

const char *
handle_access_in_pragmat (NODE_T *p, const char *pragmat, size_t pos)
{
  const char *beginning = pragmat;

  SKIP_WHITESPACES (pragmat);
  /* Parse module name.  */
  char *module;
  PARSE_WORD (pragmat, module);
  if (*module == '\0')
    {
      a68_error (p, "invalid pragmat: expected module name after %<access%>");
      return NULL;
    }
  SKIP_WHITESPACES (pragmat);
  /* Parse "in " */
  if (strncmp (pragmat, "in ", 3) != 0)
    {
      a68_error (p, "invalid pragmat: expected %<in%> after module name");
      return NULL;
    }
  pragmat += 3;
  SKIP_WHITESPACES (pragmat);
  /* Parse a string denotation.  */
  char *filename;
  pragmat = parse_string_denotation (pragmat, &filename);
  if (filename == NULL)
    {
      size_t off = pos + pragmat - beginning;
      char *found;
      PARSE_WORD (pragmat, found);
      a68_error_in_pragmat (p, off,
			    "in %<access%> pragmat, expected string, found Z",
			    found);
      return NULL;
    }

  /* Normalize module indicant to upper-case.  */
  for (char *q = module; *q; ++q)
    *q = TOUPPER (*q);

  /* Add entry in the module files map.  */
  const char **pmodule = A68_MODULE_FILES->get (module);
  if (pmodule != NULL)
    {
      a68_error_in_pragmat (p, pos + pragmat - beginning,
			    "module Z cannot appear in multiple %<access%> pragmats",
			    module);
      return NULL;
    }

  SKIP_WHITESPACES (pragmat);
  /* Skip closing PR or PRAGMAT.  */
  if (NPRAGMAT_TYPE (p) == STYLE_I_PRAGMAT_SYMBOL)
    pragmat += 2;
  else
    pragmat += 7;

  A68_MODULE_FILES->put (ggc_strdup (module), ggc_strdup (filename));
  free (filename);
  return pragmat;
}

/* Parse and action on a pragmat.  */

void
handle_pragmat (NODE_T *p)
{
  const char *pragmat = NPRAGMAT (p);
  if (pragmat != NULL
      && (NPRAGMAT_TYPE (p) == STYLE_I_PRAGMAT_SYMBOL
	  || NPRAGMAT_TYPE (p) == BOLD_PRAGMAT_SYMBOL))
    {
      /* Process pragmat commands.  */
      do
	{
	  SKIP_WHITESPACES (pragmat);
	  /* Skip PR or PRAGMAT.  */
	  if (NPRAGMAT_TYPE (p) == STYLE_I_PRAGMAT_SYMBOL)
	    pragmat += 2;
	  else
	    pragmat += 7;
	  SKIP_WHITESPACES (pragmat);
	  /* Get first word in pragmat and dispatch.  */
	  SKIP_WHITESPACES (pragmat);
	  char *word = (char *) alloca (strlen (pragmat));
	  size_t i = 0;
	  while (ISALPHA (*pragmat) || *pragmat == '\n')
	    word[i++] = *(pragmat++);
	  word[i] = '\0';

	  if (strcmp (word, "access") == 0)
	    {
	      pragmat
		= handle_access_in_pragmat (p, pragmat, pragmat - NPRAGMAT (p));
	      if (pragmat == NULL)
		break;
	    }
	  else if (strcmp (word, "include") == 0)
	    /* Include pragmats are handled in the scanner.  */
	    return;
	  else
	    {
	      a68_error_in_pragmat (p, pragmat - NPRAGMAT (p),
				    "unrecognized pragmat Z", word);
	      break;
	    }
	}
      while (*pragmat != '\0');
    }
}

/* Entry point: handle all pragmats in the given parse tree.  */

void
a68_handle_pragmats (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      a68_handle_pragmats (SUB (p));
      handle_pragmat (p);
    }
}
