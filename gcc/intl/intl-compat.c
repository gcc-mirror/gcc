/* intl-compat.c - Stub functions to call gettext functions from GNU gettext
   Library.
   Copyright (C) 1995, 2000, 2001 Software Foundation, Inc.

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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libgnuintl.h"
#include "gettextP.h"

/* @@ end of prolog @@ */

/* This file redirects the gettext functions (without prefix or suffix) to
   those defined in the included GNU gettext library (with "__" suffix).
   It is compiled into libintl when the included GNU gettext library is
   configured --with-included-gettext.

   This redirection works also in the case that the system C library or
   the system libintl library contain gettext/textdomain/... functions.
   If it didn't, we would need to add preprocessor level redirections to
   libgnuintl.h of the following form:

#    define gettext gettext__
#    define dgettext dgettext__
#    define dcgettext dcgettext__
#    define ngettext ngettext__
#    define dngettext dngettext__
#    define dcngettext dcngettext__
#    define textdomain textdomain__
#    define bindtextdomain bindtextdomain__
#    define bind_textdomain_codeset bind_textdomain_codeset__

   How does this redirection work? There are two cases.
   A. When libintl.a is linked into an executable, it works because
      functions defined in the executable always override functions in
      the shared libraries.
   B. When libintl.so is used, it works because
      1. those systems defining gettext/textdomain/... in the C library
         (namely, Solaris 2.4 and newer, and GNU libc 2.0 and newer) are
         ELF systems and define these symbols as weak, thus explicitly
         letting other shared libraries override it.
      2. those systems defining gettext/textdomain/... in a standalone
         libintl.so library (namely, Solaris 2.3 and newer) have this
         shared library in /usr/lib, and the linker will search /usr/lib
         *after* the directory where the GNU gettext library is installed.

   A third case, namely when libintl.a is linked into a shared library
   whose name is not libintl.so, is not supported. In this case, on
   Solaris, when -lintl precedes the linker option for the shared library
   containing GNU gettext, the system's gettext would indeed override
   the GNU gettext. Anyone doing this kind of stuff must be clever enough
   to 1. compile libintl.a with -fPIC, 2. remove -lintl from his linker
   command line.  */


#undef gettext
#undef dgettext
#undef dcgettext
#undef ngettext
#undef dngettext
#undef dcngettext
#undef textdomain
#undef bindtextdomain
#undef bind_textdomain_codeset


char *
gettext (msgid)
     const char *msgid;
{
  return gettext__ (msgid);
}


char *
dgettext (domainname, msgid)
     const char *domainname;
     const char *msgid;
{
  return dgettext__ (domainname, msgid);
}


char *
dcgettext (domainname, msgid, category)
     const char *domainname;
     const char *msgid;
     int category;
{
  return dcgettext__ (domainname, msgid, category);
}


char *
ngettext (msgid1, msgid2, n)
     const char *msgid1;
     const char *msgid2;
     unsigned long int n;
{
  return ngettext__ (msgid1, msgid2, n);
}


char *
dngettext (domainname, msgid1, msgid2, n)
     const char *domainname;
     const char *msgid1;
     const char *msgid2;
     unsigned long int n;
{
  return dngettext__ (domainname, msgid1, msgid2, n);
}


char *
dcngettext (domainname, msgid1, msgid2, n, category)
     const char *domainname;
     const char *msgid1;
     const char *msgid2;
     unsigned long int n;
     int category;
{
  return dcngettext__ (domainname, msgid1, msgid2, n, category);
}


char *
textdomain (domainname)
     const char *domainname;
{
  return textdomain__ (domainname);
}


char *
bindtextdomain (domainname, dirname)
     const char *domainname;
     const char *dirname;
{
  return bindtextdomain__ (domainname, dirname);
}


char *
bind_textdomain_codeset (domainname, codeset)
     const char *domainname;
     const char *codeset;
{
  return bind_textdomain_codeset__ (domainname, codeset);
}
