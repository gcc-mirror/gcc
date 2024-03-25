/* Target Newlib Definitions for OpenRISC.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by Stafford Horne.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_OR1K_ELF_H
#define GCC_OR1K_ELF_H

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc -lor1k "			\
  "%{mboard=*:-lboard-%*; :-lboard-or1ksim} --end-group"

#undef	LINK_SPEC
#define LINK_SPEC "%{h*}			\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{!static:%{rdynamic:-export-dynamic}}	\
   --entry=0x100"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

#endif /* GCC_OR1K_ELF_H */
