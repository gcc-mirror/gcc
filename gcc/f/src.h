/* src.h -- Public #include File
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

   Owning Modules:
      src.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_SRC_H
#define GCC_F_SRC_H

#include "bad.h"
#include "top.h"

extern char ffesrc_char_match_init_[256];
extern char ffesrc_char_match_noninit_[256];
extern char ffesrc_char_source_[256];
extern char ffesrc_char_internal_init_[256];
extern ffebad ffesrc_bad_symbol_init_[256];
extern ffebad ffesrc_bad_symbol_noninit_[256];
extern bool ffesrc_check_symbol_;
extern bool ffesrc_ok_match_init_upper_;
extern bool ffesrc_ok_match_init_lower_;
extern bool ffesrc_ok_match_noninit_upper_;
extern bool ffesrc_ok_match_noninit_lower_;

/* These C-language-syntax modifiers could avoid the match arg if gcc's
   extension allowing macros to generate dynamic labels was used.  They
   could use the no_match arg (and the "caller's" label defs) if there
   was a way to say "goto default" in a switch statement.  Oh well.

   NOTE: These macro assume "case FFESRC_CASE_MATCH_[NON]INIT(...):" is used
   to invoke them, and thus assume the "above" case does not fall through to
   this one.  This syntax was chosen to keep indenting tools working.  */

#define FFESRC_CASE_MATCH_INIT(upper, lower, match, no_match) \
 upper: if (!ffesrc_ok_match_init_upper_) goto no_match; \
  else goto match; \
 case lower: if (!ffesrc_ok_match_init_lower_) goto no_match; \
 match

#define FFESRC_CASE_MATCH_NONINIT(upper, lower, match, no_match) \
 upper: if (!ffesrc_ok_match_noninit_upper_) goto no_match; \
  else goto match; \
 case lower: if (!ffesrc_ok_match_noninit_lower_) goto no_match; \
 match

/* If character is ok in a symbol name (not including intrinsic names),
   returns FFEBAD, else returns something else, type ffebad.  */

#define ffesrc_bad_char_symbol_init(c) \
  (ffesrc_bad_symbol_init_[(unsigned int) (c)])
#define ffesrc_bad_char_symbol_noninit(c) \
  (ffesrc_bad_symbol_noninit_[(unsigned int) (c)])

/* Returns TRUE if character is ok in a symbol name (including
   intrinsic names).  Doesn't care about case settings, this is
   used just for parsing (before semantic complaints about symbol-
   name casing and such).  One specific usage is to decide whether
   an underscore is valid as the first or subsequent character in
   some symbol name -- if not, an underscore is a separate token
   (while lexing, for example).  Note that ffesrc_is_name_init
   must return TRUE for a (not necessarily proper) subset of
   characters for which ffelex_is_firstnamechar returns TRUE.  */

#define ffesrc_is_name_init(c) \
  ((ISALPHA ((c))) || (! (1 || ffe_is_90 ()) && ((c) == '_')))
#define ffesrc_is_name_noninit(c) \
  ((ISALNUM ((c))) || (! (1 || ffe_is_90 ()) && ((c) == '_')))

/* Test if source-translated character matches given alphabetic character
   (passed in both uppercase and lowercase, to allow for custom speedup
   of compilation in environments where compile-time options aren't needed
   for casing).	 */

#define ffesrc_char_match_init(c, up, low) \
  (ffesrc_char_match_init_[(unsigned int) (c)] == up)

#define ffesrc_char_match_noninit(c, up, low) \
  (ffesrc_char_match_noninit_[(unsigned int) (c)] == up)

/* Translate character from input-file form to source form.  */

#define ffesrc_char_source(c) (ffesrc_char_source_[(unsigned int) (c)])

/* Translate internal character (upper/lower) to source form in an
   initial-character context (i.e. ffesrc_char_match_init of the result
   will always succeed).  */

#define ffesrc_char_internal_init(up, low) \
  (ffesrc_char_internal_init_[(unsigned int) (up)])

/* Returns TRUE if a name representing a symbol should be checked for
   validity according to compile-time options.	That is, if it is possible
   that ffesrc_bad_char_symbol(c) can return something other than FFEBAD
   for any valid character in an ffelex NAME(S) token.	*/

#define ffesrc_check_symbol() ffesrc_check_symbol_

#define ffesrc_init_0()
void ffesrc_init_1 (void);
#define ffesrc_init_2()
#define ffesrc_init_3()
#define ffesrc_init_4()
int ffesrc_strcmp_1ns2i (ffeCase mcase, const char *var, int len,
			 const char *str_ic);
int ffesrc_strcmp_2c (ffeCase mcase, const char *var, const char *str_uc,
		      const char *str_lc, const char *str_ic);
int ffesrc_strncmp_2c (ffeCase mcase, const char *var, const char *str_uc,
		       const char *str_lc, const char *str_ic, int len);
#define ffesrc_terminate_0()
#define ffesrc_terminate_1()
#define ffesrc_terminate_2()
#define ffesrc_terminate_3()
#define ffesrc_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_SRC_H */
