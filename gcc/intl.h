/* intl.h - internationalization
   Copyright 1998, 2001 Free Software Foundation, Inc.

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
   02111-1307, USA.  */

#ifndef GCC_INTL_H
#define GCC_INTL_H

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

#ifndef HAVE_SETLOCALE
# define setlocale(category, locale) (locale)
#endif

#ifdef USE_INCLUDED_LIBINTL
# include <intl/libgnuintl.h>
#else
# ifdef HAVE_LIBINTL_H
#  include <libintl.h>
# else
#  undef ENABLE_NLS
# endif
#endif

#ifdef ENABLE_NLS
extern void gcc_init_libintl PARAMS ((void));
#else
/* Stubs.  */
# undef textdomain
# define textdomain(domain) (domain)
# undef bindtextdomain
# define bindtextdomain(domain, directory) (domain)
# undef gettext
# define gettext(msgid) (msgid)
# define gcc_init_libintl()	/* nothing */
#endif

#ifndef _
# define _(msgid) gettext (msgid)
#endif

#ifndef N_
# define N_(msgid) (msgid)
#endif

#endif /* intl.h */
