/* m2pp.h pretty print trees, output in Modula-2 where possible.

Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#if !defined(m2pp_h)
#define m2pp_h
#if defined(m2pp_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2pp_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2pp_c.  */

typedef enum
{
  M2PP_DUMP_STDOUT,  /* This must remain the first field.  */
  M2PP_DUMP_PRE_GENERICIZE,
  M2PP_DUMP_POST_GENERICIZE,
  M2PP_DUMP_FD,
  M2PP_DUMP_END,
} m2pp_dump_kind;

EXTERN void m2pp_CreateDumpGimple (char *template_name, int template_len);
EXTERN void m2pp_dump_gimple (m2pp_dump_kind kind, tree fndecl);
EXTERN void m2pp_CloseDumpGimple (void);
EXTERN void m2pp_DumpGimpleFd (int fd, tree fndecl);

namespace modula2 {
/* GDB Interactive interface to m2pp.  Allow a maintainer to dump
   the trees in Modula-2.  */

EXTERN void pf (tree t);
EXTERN void pe (tree t);
EXTERN void pt (tree t);
EXTERN void ptl (tree t);
EXTERN void pv (tree t);
EXTERN void ptcl (tree t);
}

#   undef EXTERN
#endif
