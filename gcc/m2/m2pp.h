/* m2pp.h pretty print trees, output in Modula-2 where possible.

Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

#if !defined(M2PP_H)
#   define M2PP_H

#   if defined(M2PP_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void pf (tree t);
EXTERN void pe (tree t);
EXTERN void pt (tree t);
EXTERN void ptl (tree t);
EXTERN void pv (tree t);
EXTERN void ptcl (tree t);


#   undef EXTERN
#endif
