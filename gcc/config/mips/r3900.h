/* Definitions of MIPS sub target machine for GNU compiler.
   Toshiba r3900.  You should include mips.h after this.

   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
   Free Software Foundation, Inc.
   Contributed by Gavin Koch (gavin@cygnus.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define MIPS_CPU_STRING_DEFAULT "R3900"
#define MIPS_ISA_DEFAULT 1

#define MULTILIB_DEFAULTS { MULTILIB_ENDIAN_DEFAULT, "msoft-float" }

/* We use the MIPS EABI by default.  */
#define MIPS_ABI_DEFAULT ABI_EABI

#define SUBTARGET_CPP_SPEC "\
%{!mabi=32: %{!mabi=n32: %{!mabi=64: -D__mips_eabi}}} \
%{!msingle-float:-D__mips_soft_float} \
%{mhard-float:%e-mhard-float not supported} \
%{msingle-float:%{msoft-float: \
  %e-msingle-float and -msoft-float can not both be specified}}"

/* by default (if not mips-something-else) produce code for the r3900 */
#define SUBTARGET_CC1_SPEC "\
%{mhard-float:%e-mhard-float not supported} \
%{msingle-float:%{msoft-float: \
  %e-msingle-float and -msoft-float can not both be specified}}"

/* Debugging */

#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* For the 'preferred' cases ("gN" and "ggdbN") we need to tell the
   gnu assembler not to generate debugging information.  */

#define SUBTARGET_ASM_DEBUGGING_SPEC "\
%{!mmips-as: \
  %{g:-g0} %{g0:-g0} %{g1:-g0} %{g2:-g0} %{g3:-g0} \
  %{ggdb:-g0} %{ggdb0:-g0} %{ggdb1:-g0} %{ggdb2:-g0} %{ggdb3:-g0} \
  %{gdwarf-2*:-g0}} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"

/* eof */
