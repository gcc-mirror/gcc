/* Definitions for rtems targeting a v850 using ELF.
   Copyright (C) 2012-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Specify predefined symbols in preprocessor.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define( "__rtems__" );		\
      builtin_assert( "system=rtems" );		\
    }						\
  while (0)

/* Map mv850e1 and mv850es to mv850e to match MULTILIB_MATCHES */
#undef  ASM_SPEC
#define ASM_SPEC "%{mv850es:-mv850e} \
%{mv850e1:-mv850e} \
%{!mv850es:%{!mv850e1:%{mv*:-mv%*}} \
%{m8byte-align:-m8byte-align} \
%{mgcc-abi:-mgcc-abi}}"
