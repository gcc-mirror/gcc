/* src.c -- Implementation File
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Related Modules:

   Description:
      Source-file functions to handle various combinations of case sensitivity
      and insensitivity at run time.

   Modifications:
*/

#include "proj.h"
#include "src.h"
#include "top.h"

/* This array does a toupper (), but any valid char type is valid as an
   index and returns identity if not a lower-case character.  */

char ffesrc_toupper_[256];

/* This array does a tolower (), but any valid char type is valid as an
   index and returns identity if not an upper-case character.  */

char ffesrc_tolower_[256];

/* This array is set up so that, given a source-mapped character, the result
   of indexing into this array will match an upper-cased character depending
   on the source-mapped character's case and the established ffe_case_match()
   setting.  So the uppercase cells contain identies (e.g. ['A'] == 'A')
   as long as uppercase matching is permitted (!FFE_caseLOWER) and the
   lowercase cells contain uppercased identities (e.g. ['a'] == 'A') as long
   as lowercase matching is permitted (!FFE_caseUPPER).	 Else the case
   cells contain -1.  _init_ is for the first character of a keyword,
   and _noninit_ is for other characters.  */

char ffesrc_char_match_init_[256];
char ffesrc_char_match_noninit_[256];

/* This array is used to map input source according to the established
   ffe_case_source() setting: for FFE_caseNONE, the array is all
   identities; for FFE_caseUPPER, the lowercase cells contain
   uppercased identities; and vice versa for FFE_caseLOWER.  */

char ffesrc_char_source_[256];

/* This array is used to map an internally generated character so that it
   will be accepted as an initial character in a keyword.  The assumption
   is that the incoming character is uppercase.  */

char ffesrc_char_internal_init_[256];

/* This array is used to determine if a particular character is valid in
   a symbol name according to the established ffe_case_symbol() setting:
   for FFE_caseNONE, the array is all FFEBAD; for FFE_caseUPPER, the
   lowercase cells contain a non-FFEBAD error code (FFEBAD_SYMBOL_UPPER_CASE);
   and vice versa for FFE_caseLOWER.  _init_ and _noninit_ distinguish
   between initial and subsequent characters for the caseINITCAP case,
   and their error codes are different for appropriate messages --
   specifically, _noninit_ contains a non-FFEBAD error code for all
   except lowercase characters for the caseINITCAP case.

   See ffesrc_check_symbol_, it must be TRUE if this array is not all
   FFEBAD.  */

ffebad ffesrc_bad_symbol_init_[256];
ffebad ffesrc_bad_symbol_noninit_[256];

/* Set TRUE if any element in ffesrc_bad_symbol (with an index representing
   a character that can also be in the text of a token passed to
   ffename_find, strictly speaking) is not FFEBAD.  I.e., TRUE if it is
   necessary to check token characters against the ffesrc_bad_symbol_
   array.  */

bool ffesrc_check_symbol_;

/* These are set TRUE if the kind of character (upper/lower) is ok as a match
   in the context (initial/noninitial character of keyword).  */

bool ffesrc_ok_match_init_upper_;
bool ffesrc_ok_match_init_lower_;
bool ffesrc_ok_match_noninit_upper_;
bool ffesrc_ok_match_noninit_lower_;

/* Initialize table of alphabetic matches. */

void
ffesrc_init_1 ()
{
  int i;

  for (i = 0; i < 256; ++i)
    {
      ffesrc_char_match_init_[i] = i;
      ffesrc_char_match_noninit_[i] = i;
      ffesrc_char_source_[i] = i;
      ffesrc_char_internal_init_[i] = i;
      ffesrc_toupper_[i] = i;
      ffesrc_tolower_[i] = i;
      ffesrc_bad_symbol_init_[i] = FFEBAD;
      ffesrc_bad_symbol_noninit_[i] = FFEBAD;
    }

  for (i = 'A'; i <= 'Z'; ++i)
    ffesrc_tolower_[i] = tolower (i);

  for (i = 'a'; i <= 'z'; ++i)
    ffesrc_toupper_[i] = toupper (i);

  ffesrc_check_symbol_ = (ffe_case_symbol () != FFE_caseNONE);

  ffesrc_ok_match_init_upper_ = (ffe_case_match () != FFE_caseLOWER);
  ffesrc_ok_match_init_lower_ = (ffe_case_match () != FFE_caseUPPER)
    && (ffe_case_match () != FFE_caseINITCAP);
  ffesrc_ok_match_noninit_upper_ = (ffe_case_match () != FFE_caseLOWER)
    && (ffe_case_match () != FFE_caseINITCAP);
  ffesrc_ok_match_noninit_lower_ = (ffe_case_match () != FFE_caseUPPER);

  /* Note that '-' is used to flag an invalid match character.	'-' is
     somewhat arbitrary, actually.  -1 was used, but that's not wise on a
     system with unsigned chars as default -- it'd turn into 255 or some such
     large positive number, which would sort higher than the alphabetics and
     thus possibly cause problems.  So '-' is picked just because it's never
     likely to be a symbol character in Fortran and because it's "less than"
     any alphabetic character.	EBCDIC might see things differently, I don't
     remember it well enough, but that's just tough -- lots of other things
     might have to change to support EBCDIC -- anyway, some other character
     could easily be picked.  */

#define FFESRC_INVALID_SYMBOL_CHAR_ '-'

  if (!ffesrc_ok_match_init_upper_)
    for (i = 'A'; i <= 'Z'; ++i)
      ffesrc_char_match_init_[i] = FFESRC_INVALID_SYMBOL_CHAR_;

  if (ffesrc_ok_match_init_lower_)
    for (i = 'a'; i <= 'z'; ++i)
      ffesrc_char_match_init_[i] = toupper (i);
  else
    for (i = 'a'; i <= 'z'; ++i)
      ffesrc_char_match_init_[i] = FFESRC_INVALID_SYMBOL_CHAR_;

  if (!ffesrc_ok_match_noninit_upper_)
    for (i = 'A'; i <= 'Z'; ++i)
      ffesrc_char_match_noninit_[i] = FFESRC_INVALID_SYMBOL_CHAR_;

  if (ffesrc_ok_match_noninit_lower_)
    for (i = 'a'; i <= 'z'; ++i)
      ffesrc_char_match_noninit_[i] = toupper (i);
  else
    for (i = 'a'; i <= 'z'; ++i)
      ffesrc_char_match_noninit_[i] = FFESRC_INVALID_SYMBOL_CHAR_;

  if (ffe_case_source () == FFE_caseLOWER)
    for (i = 'A'; i <= 'Z'; ++i)
      ffesrc_char_source_[i] = tolower (i);
  else if (ffe_case_source () == FFE_caseUPPER)
    for (i = 'a'; i <= 'z'; ++i)
      ffesrc_char_source_[i] = toupper (i);

  if (ffe_case_match () == FFE_caseLOWER)
    for (i = 'A'; i <= 'Z'; ++i)
      ffesrc_char_internal_init_[i] = tolower (i);

  switch (ffe_case_symbol ())
    {
    case FFE_caseLOWER:
      for (i = 'A'; i <= 'Z'; ++i)
	{
	  ffesrc_bad_symbol_init_[i] = FFEBAD_SYMBOL_UPPER_CASE;
	  ffesrc_bad_symbol_noninit_[i] = FFEBAD_SYMBOL_UPPER_CASE;
	}
      break;

    case FFE_caseUPPER:
      for (i = 'a'; i <= 'z'; ++i)
	{
	  ffesrc_bad_symbol_init_[i] = FFEBAD_SYMBOL_LOWER_CASE;
	  ffesrc_bad_symbol_noninit_[i] = FFEBAD_SYMBOL_LOWER_CASE;
	}
      break;

    case FFE_caseINITCAP:
      for (i = 0; i < 256; ++i)
	ffesrc_bad_symbol_noninit_[i] = FFEBAD_SYMBOL_NOLOWER_INITCAP;
      for (i = 'a'; i <= 'z'; ++i)
	{
	  ffesrc_bad_symbol_init_[i] = FFEBAD_SYMBOL_LOWER_INITCAP;
	  ffesrc_bad_symbol_noninit_[i] = FFEBAD;
	}
      break;

    default:
      break;
    }
}

/* Compare two strings a la strcmp, the first being a source string with its
   length passed, and the second being a constant string passed
   in InitialCaps form.	 Also, the return value is always -1, 0, or 1. */

int
ffesrc_strcmp_1ns2i (ffeCase mcase, const char *var, int len,
		     const char *str_ic)
{
  char c;
  char d;

  switch (mcase)
    {
    case FFE_caseNONE:
      for (; len > 0; --len, ++var, ++str_ic)
	{
	  c = ffesrc_char_source (*var);	/* Transform source. */
	  c = ffesrc_toupper (c);	/* Upcase source. */
	  d = ffesrc_toupper (*str_ic);	/* Upcase InitialCaps char. */
	  if (c != d)
	    {
	      if ((d != '\0') && (c < d))
		return -1;
	      else
		return 1;
	    }
	}
      break;

    case FFE_caseUPPER:
      for (; len > 0; --len, ++var, ++str_ic)
	{
	  c = ffesrc_char_source (*var);	/* Transform source. */
	  d = ffesrc_toupper (*str_ic);	/* Transform InitialCaps char. */
	  if (c != d)
	    {
	      if ((d != '\0') && (c < d))
		return -1;
	      else
		return 1;
	    }
	}
      break;

    case FFE_caseLOWER:
      for (; len > 0; --len, ++var, ++str_ic)
	{
	  c = ffesrc_char_source (*var);	/* Transform source. */
	  d = ffesrc_tolower (*str_ic);	/* Transform InitialCaps char. */
	  if (c != d)
	    {
	      if ((d != '\0') && (c < d))
		return -1;
	      else
		return 1;
	    }
	}
      break;

    case FFE_caseINITCAP:
      for (; len > 0; --len, ++var, ++str_ic)
	{
	  c = ffesrc_char_source (*var);	/* Transform source. */
	  d = *str_ic;		/* No transform of InitialCaps char. */
	  if (c != d)
	    {
	      c = ffesrc_toupper (c);
	      d = ffesrc_toupper (d);
	      while ((len > 0) && (c == d))
		{		/* Skip past equivalent (case-ins) chars. */
		  --len, ++var, ++str_ic;
		  if (len > 0)
		    c = ffesrc_toupper (*var);
		  d = ffesrc_toupper (*str_ic);
		}
	      if ((d != '\0') && (c < d))
		return -1;
	      else
		return 1;
	    }
	}
      break;

    default:
      assert ("bad case value" == NULL);
      return -1;
    }

  if (*str_ic == '\0')
    return 0;
  return -1;
}

/* Compare two strings a la strcmp, the second being a constant string passed
   in both uppercase and lowercase form.  If not equal, the uppercase string
   is used to determine the sign of the return value.  Also, the return
   value is always -1, 0, or 1. */

int
ffesrc_strcmp_2c (ffeCase mcase, const char *var, const char *str_uc,
		  const char *str_lc, const char *str_ic)
{
  int i;
  char c;

  switch (mcase)
    {
    case FFE_caseNONE:
      for (; *var != '\0'; ++var, ++str_uc)
	{
	  c = ffesrc_toupper (*var);	/* Upcase source. */
	  if (c != *str_uc)
	    {
	      if ((*str_uc != '\0') && (c < *str_uc))
		return -1;
	      else
		return 1;
	    }
	}
      if (*str_uc == '\0')
	return 0;
      return -1;

    case FFE_caseUPPER:
      i = strcmp (var, str_uc);
      break;

    case FFE_caseLOWER:
      i = strcmp (var, str_lc);
      break;

    case FFE_caseINITCAP:
      for (; *var != '\0'; ++var, ++str_ic, ++str_uc)
	{
	  if (*var != *str_ic)
	    {
	      c = ffesrc_toupper (*var);
	      while ((c != '\0') && (c == *str_uc))
		{		/* Skip past equivalent (case-ins) chars. */
		  ++var, ++str_uc;
		  c = ffesrc_toupper (*var);
		}
	      if ((*str_uc != '\0') && (c < *str_uc))
		return -1;
	      else
		return 1;
	    }
	}
      if (*str_ic == '\0')
	return 0;
      return -1;

    default:
      assert ("bad case value" == NULL);
      return -1;
    }

  if (i == 0)
    return 0;
  else if (i < 0)
    return -1;
  return 1;
}

/* Compare two strings a la strncmp, the second being a constant string passed
   in uppercase, lowercase, and InitialCaps form.  If not equal, the
   uppercase string is used to determine the sign of the return value.	*/

int
ffesrc_strncmp_2c (ffeCase mcase, const char *var, const char *str_uc,
		   const char *str_lc, const char *str_ic, int len)
{
  int i;
  char c;

  switch (mcase)
    {
    case FFE_caseNONE:
      for (; len > 0; ++var, ++str_uc, --len)
	{
	  c = ffesrc_toupper (*var);	/* Upcase source. */
	  if (c != *str_uc)
	    {
	      if (c < *str_uc)
		return -1;
	      else
		return 1;
	    }
	}
      return 0;

    case FFE_caseUPPER:
      i = strncmp (var, str_uc, len);
      break;

    case FFE_caseLOWER:
      i = strncmp (var, str_lc, len);
      break;

    case FFE_caseINITCAP:
      for (; len > 0; ++var, ++str_ic, ++str_uc, --len)
	{
	  if (*var != *str_ic)
	    {
	      c = ffesrc_toupper (*var);
	      while ((len > 0) && (c == *str_uc))
		{		/* Skip past equivalent (case-ins) chars. */
		  --len, ++var, ++str_uc;
		  if (len > 0)
		    c = ffesrc_toupper (*var);
		}
	      if ((len > 0) && (c < *str_uc))
		return -1;
	      else
		return 1;
	    }
	}
      return 0;

    default:
      assert ("bad case value" == NULL);
      return -1;
    }

  if (i == 0)
    return 0;
  else if (i < 0)
    return -1;
  return 1;
}
