/* Message catalogs for internationalization.
   Copyright (C) 1995-1997, 2000, 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */

#ifndef _LIBINTL_H
#define _LIBINTL_H	1

#include <locale.h>

/* The LC_MESSAGES locale category is the category used by the functions
   gettext() and dgettext().  It is specified in POSIX, but not in ANSI C.
   On systems that don't define it, use an arbitrary value instead.
   On Solaris, <locale.h> defines __LOCALE_H then includes <libintl.h> (i.e.
   this file!) and then only defines LC_MESSAGES.  To avoid a redefinition
   warning, don't define LC_MESSAGES in this case.  */
#if !defined LC_MESSAGES && !defined __LOCALE_H
# define LC_MESSAGES 1729
#endif

/* We define an additional symbol to signal that we use the GNU
   implementation of gettext.  */
#define __USE_GNU_GETTEXT 1

/* Resolve a platform specific conflict on DJGPP.  GNU gettext takes
   precedence over _conio_gettext.  */
#ifdef __DJGPP__
# undef gettext
# define gettext gettext
#endif

#ifndef PARAMS
# if __STDC__ || defined __cplusplus
#  define PARAMS(args) args
# else
#  define PARAMS(args) ()
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Look up MSGID in the current default message catalog for the current
   LC_MESSAGES locale.  If not found, returns MSGID itself (the default
   text).  */
extern char *gettext PARAMS ((const char *__msgid));

/* Look up MSGID in the DOMAINNAME message catalog for the current
   LC_MESSAGES locale.  */
extern char *dgettext PARAMS ((const char *__domainname, const char *__msgid));

/* Look up MSGID in the DOMAINNAME message catalog for the current CATEGORY
   locale.  */
extern char *dcgettext PARAMS ((const char *__domainname, const char *__msgid,
				int __category));


/* Similar to `gettext' but select the plural form corresponding to the
   number N.  */
extern char *ngettext PARAMS ((const char *__msgid1, const char *__msgid2,
			       unsigned long int __n));

/* Similar to `dgettext' but select the plural form corresponding to the
   number N.  */
extern char *dngettext PARAMS ((const char *__domainname, const char *__msgid1,
				const char *__msgid2, unsigned long int __n));

/* Similar to `dcgettext' but select the plural form corresponding to the
   number N.  */
extern char *dcngettext PARAMS ((const char *__domainname, const char *__msgid1,
				 const char *__msgid2, unsigned long int __n,
				 int __category));


/* Set the current default message catalog to DOMAINNAME.
   If DOMAINNAME is null, return the current default.
   If DOMAINNAME is "", reset to the default of "messages".  */
extern char *textdomain PARAMS ((const char *__domainname));

/* Specify that the DOMAINNAME message catalog will be found
   in DIRNAME rather than in the system locale data base.  */
extern char *bindtextdomain PARAMS ((const char *__domainname,
				     const char *__dirname));

/* Specify the character encoding in which the messages from the
   DOMAINNAME message catalog will be returned.  */
extern char *bind_textdomain_codeset PARAMS ((const char *__domainname,
					      const char *__codeset));


/* Optimized version of the functions above.  */
#if defined __OPTIMIZED
/* These are macros, but could also be inline functions.  */

# define gettext(msgid)							      \
  dgettext (NULL, msgid)

# define dgettext(domainname, msgid)					      \
  dcgettext (domainname, msgid, LC_MESSAGES)

# define ngettext(msgid1, msgid2, n)					      \
  dngettext (NULL, msgid1, msgid2, n)

# define dngettext(domainname, msgid1, msgid2, n)			      \
  dcngettext (domainname, msgid1, msgid2, n, LC_MESSAGES)

#endif /* Optimizing. */


#ifdef __cplusplus
}
#endif

#endif /* libintl.h */
