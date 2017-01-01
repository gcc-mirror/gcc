/* Message translation utilities.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "intl.h"

#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

/* Opening quotation mark for diagnostics.  */
const char *open_quote = "'";

/* Closing quotation mark for diagnostics.  */
const char *close_quote = "'";

/* The name of the locale encoding.  */
const char *locale_encoding = NULL;

/* Whether the locale is using UTF-8.  */
bool locale_utf8 = false;

#ifdef ENABLE_NLS

/* Initialize the translation library for GCC.  This performs the
   appropriate sequence of calls - setlocale, bindtextdomain,
   textdomain.  LC_CTYPE determines the character set used by the
   terminal, so it has be set to output messages correctly.  */

void
gcc_init_libintl (void)
{
#ifdef HAVE_LC_MESSAGES
  setlocale (LC_CTYPE, "");
  setlocale (LC_MESSAGES, "");
#else
  setlocale (LC_ALL, "");
#endif

  (void) bindtextdomain ("gcc", LOCALEDIR);
  (void) textdomain ("gcc");

  /* Opening quotation mark.  */
  open_quote = _("`");

  /* Closing quotation mark.  */
  close_quote = _("'");

#if defined HAVE_LANGINFO_CODESET
  locale_encoding = nl_langinfo (CODESET);
  if (locale_encoding != NULL
      && (!strcasecmp (locale_encoding, "utf-8")
	  || !strcasecmp (locale_encoding, "utf8")))
    locale_utf8 = true;
#endif

  if (!strcmp (open_quote, "`") && !strcmp (close_quote, "'"))
    {
      /* Untranslated quotes that it may be possible to replace with
	 U+2018 and U+2019; but otherwise use "'" instead of "`" as
	 opening quote.  */
      open_quote = "'";
#if defined HAVE_LANGINFO_CODESET
      if (locale_utf8)
	{
	  open_quote = "\xe2\x80\x98";
	  close_quote = "\xe2\x80\x99";
	}
#endif
    }
}

#if defined HAVE_WCHAR_H && defined HAVE_WORKING_MBSTOWCS && defined HAVE_WCSWIDTH
#include <wchar.h>

/* Returns the width in columns of MSGSTR, which came from gettext.
   This is for indenting subsequent output.  */

size_t
gcc_gettext_width (const char *msgstr)
{
  size_t nwcs = mbstowcs (0, msgstr, 0);
  wchar_t *wmsgstr = XALLOCAVEC (wchar_t, nwcs + 1);

  mbstowcs (wmsgstr, msgstr, nwcs + 1);
  return wcswidth (wmsgstr, nwcs);
}

#else  /* no wcswidth */

/* We don't have any way of knowing how wide the string is.  Guess
   the length of the string.  */

size_t
gcc_gettext_width (const char *msgstr)
{
  return strlen (msgstr);
}

#endif

#endif /* ENABLE_NLS */

#ifndef ENABLE_NLS

const char *
fake_ngettext (const char *singular, const char *plural, unsigned long n)
{
  if (n == 1UL)
    return singular;

  return plural;
}

#endif

/* Return the indent for successive lines, using the width of
   the STR.  STR must have been translated already.  The string
   must be freed by the caller.  */

char *
get_spaces (const char *str)
{
   size_t len = gcc_gettext_width (str);
   char *spaces = XNEWVEC (char, len + 1);
   memset (spaces, ' ', len);
   spaces[len] = '\0';
   return spaces;
}



