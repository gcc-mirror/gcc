/* m2lexbuf.h provides a minimal interface to a modula-2 m2lexbuf.

Copyright (C) 2026 Free Software Foundation, Inc.
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

#if !defined(m2lexbuf_h)

#define m2lexbuf_h
#if defined(m2lexbuf_c)
#define EXTERN
#else /* !m2lexbuf_c.  */
#define EXTERN extern
#endif /* !m2lexbuf_c.  */

#define M2LexBuf_UnknownTokenNo 0
#define M2LexBuf_BuiltinTokenNo 1

EXTERN unsigned int M2LexBuf_GetBuiltinToken (void);
EXTERN unsigned int M2LexBuf_GetUnknownToken (void);

#undef EXTERN
#endif  /* !m2lexbuf_h.  */
