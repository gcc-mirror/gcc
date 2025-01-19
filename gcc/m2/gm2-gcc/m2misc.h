/* m2misc.h header file for m2misc.cc.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if !defined(m2misc_h)

#define m2misc_h
#if defined(m2misc_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else  /* !m2misc_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif  /* !m2misc_c.  */

EXTERN void m2misc_DebugTree (tree t);
EXTERN void m2misc_printStmt (void);
EXTERN void m2misc_DebugTreeChain (tree t);
EXTERN void m2misc_cerror (const char *message);
EXTERN void m2misc_error (const char *message);
EXTERN void m2misc_warning_m2_dump_filter (const char *message, const char *rule);

#undef EXTERN
#endif /* m2misc_h.  */
