/* m2langdump.h header file for m2langdump.cc.

Copyright (C) 2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

#if !defined(m2langdump_h)
#define m2langdump_h
#if defined(m2langdump_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2langdump_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2langdump_c.  */

EXTERN bool M2LangDump_IsDumpRequiredTree (tree fndecl, bool defaultvalue);

#undef EXTERN
#endif  /* m2langdump_h.  */
