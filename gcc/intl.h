/* intl.h - internationalization
   Copyright 1998 Free Software Foundation, Inc.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA. */

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

#ifndef HAVE_SETLOCALE
# define setlocale(category, locale) (locale)
#endif

#ifdef ENABLE_NLS
# include <libintl.h>
  extern const char localedir[];
#else
/* Stubs that do something close enough.  */
# ifdef textdomain
#  undef textdomain
# endif
# define textdomain(domain) (domain)
# ifdef bindtextdomain
#  undef bindtextdomain
# endif
# define bindtextdomain(domain, directory) (domain)
# ifdef gettext
#  undef gettext
# endif
# define gettext(msgid) (msgid)
#endif

#ifndef _
# define _(msgid) gettext (msgid)
#endif

#ifndef N_
# define N_(msgid) (msgid)
#endif
