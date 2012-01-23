/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            A D A D E C O D E                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 2001-2012, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/


#if defined(IN_RTS)
#include "tconfig.h"
#include "tsystem.h"
#elif defined(IN_GCC)
#include "config.h"
#include "system.h"
#endif

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "adaint.h"  /* for a macro version of xstrdup.  */

#ifndef ISDIGIT
#define ISDIGIT(c) isdigit(c)
#endif

#ifndef PARMS
#define PARMS(ARGS) ARGS
#endif

#include "adadecode.h"

static void add_verbose (const char *, char *);
static int has_prefix (const char *, const char *);
static int has_suffix (const char *, const char *);

/* This is a safe version of strcpy that can be used with overlapped
   pointers. Does nothing if s2 <= s1.  */
static void ostrcpy (char *s1, char *s2);

/* Set to nonzero if we have written any verbose info.  */
static int verbose_info;

/* Add TEXT to end of ADA_NAME, putting a leading " (" or ", ", depending
   on VERBOSE_INFO.  */

static void add_verbose (const char *text, char *ada_name)
{
  strcat (ada_name, verbose_info ? ", " : " (");
  strcat (ada_name, text);

  verbose_info = 1;
}

/* Returns 1 if NAME starts with PREFIX.  */

static int
has_prefix (const char *name, const char *prefix)
{
  return strncmp (name, prefix, strlen (prefix)) == 0;
}

/* Returns 1 if NAME ends with SUFFIX.  */

static int
has_suffix (const char *name, const char *suffix)
{
  int nlen = strlen (name);
  int slen = strlen (suffix);

  return nlen > slen && strncmp (name + nlen - slen, suffix, slen) == 0;
}

/* Safe overlapped pointers version of strcpy.  */

static void
ostrcpy (char *s1, char *s2)
{
  if (s2 > s1)
    {
      while (*s2) *s1++ = *s2++;
      *s1 = '\0';
    }
}

/* This function will return the Ada name from the encoded form.
   The Ada coding is done in exp_dbug.ads and this is the inverse function.
   see exp_dbug.ads for full encoding rules, a short description is added
   below. Right now only objects and routines are handled. Ada types are
   stripped of their encodings.

   CODED_NAME is the encoded entity name.

   ADA_NAME is a pointer to a buffer, it will receive the Ada name. A safe
   size for this buffer is: strlen (coded_name) * 2 + 60. (60 is for the
   verbose information).

   VERBOSE is nonzero if more information about the entity is to be
   added at the end of the Ada name and surrounded by ( and ).

     Coded name           Ada name                verbose info
  ---------------------------------------------------------------------
  _ada_xyz                xyz                     library level
  x__y__z                 x.y.z
  x__yTKB                 x.y                     task body
  x__yB                   x.y                     task body
  x__yX                   x.y                     body nested
  x__yXb                  x.y                     body nested
  xTK__y                  x.y                     in task
  x__y$2                  x.y                     overloaded
  x__y__3                 x.y                     overloaded
  x__Oabs                 "abs"
  x__Oand                 "and"
  x__Omod                 "mod"
  x__Onot                 "not"
  x__Oor                  "or"
  x__Orem                 "rem"
  x__Oxor                 "xor"
  x__Oeq                  "="
  x__One                  "/="
  x__Olt                  "<"
  x__Ole                  "<="
  x__Ogt                  ">"
  x__Oge                  ">="
  x__Oadd                 "+"
  x__Osubtract            "-"
  x__Oconcat              "&"
  x__Omultiply            "*"
  x__Odivide              "/"
  x__Oexpon               "**"     */

void
__gnat_decode (const char *coded_name, char *ada_name, int verbose)
{
  int lib_subprog = 0;
  int overloaded = 0;
  int task_body = 0;
  int in_task = 0;
  int body_nested = 0;

  /* Deal with empty input early.  This allows assuming non-null length
     later on, simplifying coding.  In principle, it should be our callers
     business not to call here for empty inputs.  It is easy enough to
     allow it, however, and might allow simplifications upstream so is not
     a bad thing per se.  We need a guard in any case.  */

  if (*coded_name == '\0')
    {
      *ada_name = '\0';
      return;
    }

  /* Check for library level subprogram.  */
  else if (has_prefix (coded_name, "_ada_"))
    {
      strcpy (ada_name, coded_name + 5);
      lib_subprog = 1;
    }
  else
    strcpy (ada_name, coded_name);

  /* Check for the first triple underscore in the name. This indicates
     that the name represents a type with encodings; in this case, we
     need to strip the encodings.  */
  {
    char *encodings;

    if ((encodings = (char *) strstr (ada_name, "___")) != NULL)
      {
	*encodings = '\0';
      }
  }

  /* Check for task body.  */
  if (has_suffix (ada_name, "TKB"))
    {
      ada_name[strlen (ada_name) - 3] = '\0';
      task_body = 1;
    }

  if (has_suffix (ada_name, "B"))
    {
      ada_name[strlen (ada_name) - 1] = '\0';
      task_body = 1;
    }

  /* Check for body-nested entity: X[bn] */
  if (has_suffix (ada_name, "X"))
    {
      ada_name[strlen (ada_name) - 1] = '\0';
      body_nested = 1;
    }

  if (has_suffix (ada_name, "Xb"))
    {
      ada_name[strlen (ada_name) - 2] = '\0';
      body_nested = 1;
    }

  if (has_suffix (ada_name, "Xn"))
    {
      ada_name[strlen (ada_name) - 2] = '\0';
      body_nested = 1;
    }

  /* Change instance of TK__ (object declared inside a task) to __.  */
  {
    char *tktoken;

    while ((tktoken = (char *) strstr (ada_name, "TK__")) != NULL)
      {
	ostrcpy (tktoken, tktoken + 2);
	in_task = 1;
      }
  }

  /* Check for overloading: name terminated by $nn or __nn.  */
  {
    int len = strlen (ada_name);
    int n_digits = 0;

    if (len > 1)
      while (ISDIGIT ((int) ada_name[(int) len - 1 - n_digits]))
	n_digits++;

    /* Check if we have $ or __ before digits.  */
    if (ada_name[len - 1 - n_digits] == '$')
      {
	ada_name[len - 1 - n_digits] = '\0';
	overloaded = 1;
      }
    else if (ada_name[len - 1 - n_digits] == '_'
	     && ada_name[len - 1 - n_digits - 1] == '_')
      {
	ada_name[len - 1 - n_digits - 1] = '\0';
	overloaded = 1;
      }
  }

  /* Check for nested subprogram ending in .nnnn and strip suffix. */
  {
    int last = strlen (ada_name) - 1;

    while (ISDIGIT (ada_name[last]) && last > 0)
      {
        last--;
      }

    if (ada_name[last] == '.')
      {
        ada_name[last] = (char) 0;
      }
  }

  /* Change all "__" to ".". */
  {
    int len = strlen (ada_name);
    int k = 0;

    while (k < len)
      {
	if (ada_name[k] == '_' && ada_name[k+1] == '_')
	  {
	    ada_name[k] = '.';
	    ostrcpy (ada_name + k + 1, ada_name + k + 2);
	    len = len - 1;
	  }
	k++;
      }
  }

  /* Checks for operator name.  */
  {
    const char *trans_table[][2]
      = {{"Oabs", "\"abs\""},  {"Oand", "\"and\""},    {"Omod", "\"mod\""},
	 {"Onot", "\"not\""},  {"Oor", "\"or\""},      {"Orem", "\"rem\""},
	 {"Oxor", "\"xor\""},  {"Oeq", "\"=\""},       {"One", "\"/=\""},
	 {"Olt", "\"<\""},     {"Ole", "\"<=\""},      {"Ogt", "\">\""},
	 {"Oge", "\">=\""},    {"Oadd", "\"+\""},      {"Osubtract", "\"-\""},
	 {"Oconcat", "\"&\""}, {"Omultiply", "\"*\""}, {"Odivide", "\"/\""},
	 {"Oexpon", "\"**\""}, {NULL, NULL} };
    int k = 0;

    while (1)
      {
	char *optoken;

	if ((optoken = (char *) strstr (ada_name, trans_table[k][0])) != NULL)
	  {
	    int codedlen = strlen (trans_table[k][0]);
	    int oplen = strlen (trans_table[k][1]);

	    if (codedlen > oplen)
	      /* We shrink the space.  */
	      ostrcpy (optoken, optoken + codedlen - oplen);
	    else if (oplen > codedlen)
	      {
		/* We need more space.  */
		int len = strlen (ada_name);
		int space = oplen - codedlen;
		int num_to_move = &ada_name[len] - optoken;
		int t;

		for (t = 0; t < num_to_move; t++)
		  ada_name[len + space - t - 1] = ada_name[len - t - 1];
	      }

	    /* Write symbol in the space.  */
	    strncpy (optoken, trans_table[k][1], oplen);
	  }
	else
	  k++;

	/* Check for table's ending.  */
	if (trans_table[k][0] == NULL)
	  break;
      }
  }

  /* If verbose mode is on, we add some information to the Ada name.  */
  if (verbose)
    {
      if (overloaded)
	add_verbose ("overloaded", ada_name);

      if (lib_subprog)
	add_verbose ("library level", ada_name);

      if (body_nested)
	add_verbose ("body nested", ada_name);

      if (in_task)
	add_verbose ("in task", ada_name);

      if (task_body)
	add_verbose ("task body", ada_name);

      if (verbose_info == 1)
	strcat (ada_name, ")");
    }
}

#ifdef __cplusplus
extern "C" {
#endif

#ifdef IN_GCC
char *
ada_demangle (const char *coded_name)
{
  char ada_name[2048];

  __gnat_decode (coded_name, ada_name, 0);
  return xstrdup (ada_name);
}
#endif

void
get_encoding (const char *coded_name, char *encoding)
{
  char * dest_index = encoding;
  const char *p;
  int found = 0;
  int count = 0;

  /* The heuristics is the following: we assume that the first triple
     underscore in an encoded name indicates the beginning of the
     first encoding, and that subsequent triple underscores indicate
     the next encodings. We assume that the encodings are always at the
     end of encoded names.  */

  for (p = coded_name; *p != '\0'; p++)
    {
      if (*p != '_')
	count = 0;
      else
	if (++count == 3)
	  {
	    count = 0;

	    if (found)
	      {
		dest_index = dest_index - 2;
		*dest_index++ = ':';
	      }

	    p++;
	    found = 1;
	  }

      if (found)
	*dest_index++ = *p;
    }

  *dest_index = '\0';
}

#ifdef __cplusplus
}
#endif
