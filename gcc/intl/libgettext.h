/* Convenience header for conditional use of GNU <libintl.h>.
   Copyright (C) 1995-1998, 2000, 2001 Free Software Foundation, Inc.

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

#ifndef _LIBGETTEXT_H
#define _LIBGETTEXT_H 1

/* NLS can be disabled through the configure --disable-nls option.  */
#if ENABLE_NLS

/* Get declarations of GNU message catalog functions.  */
# include <libintl.h>

#else

# define gettext(Msgid) (Msgid)
# define dgettext(Domainname, Msgid) (Msgid)
# define dcgettext(Domainname, Msgid, Category) (Msgid)
# define ngettext(Msgid1, Msgid2, N) \
    ((N) == 1 ? (char *) (Msgid1) : (char *) (Msgid2))
# define dngettext(Domainname, Msgid1, Msgid2, N) \
    ((N) == 1 ? (char *) (Msgid1) : (char *) (Msgid2))
# define dcngettext(Domainname, Msgid1, Msgid2, N, Category) \
    ((N) == 1 ? (char *) (Msgid1) : (char *) (Msgid2))
# define textdomain(Domainname) ((char *) (Domainname))
# define bindtextdomain(Domainname, Dirname) ((char *) (Dirname))
# define bind_textdomain_codeset(Domainname, Codeset) ((char *) (Codeset))

#endif

/* For automatical extraction of messages sometimes no real
   translation is needed.  Instead the string itself is the result.  */
#define gettext_noop(Str) (Str)

#endif /* _LIBGETTEXT_H */
