/* Support for GCC on Xilinx embedded PowerPC systems
   Copyright (C) 2008-2013 Free Software Foundation, Inc.
   Contributed by Michael Eager, eager@eagercon.com

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

/* Set defaults for Xilinx embedded target boards. */

#undef  CPP_SPEC
#define CPP_SPEC "\
-mxilinx-fpu                                    \
%{mfpu=sp_lite: -DHAVE_XFPU_SP_LITE}            \
%{mfpu=sp_full: -DHAVE_XFPU_SP_FULL}            \
%{mfpu=dp_lite: -DHAVE_XFPU_DP_LITE}            \
%{mfpu=dp_full: -DHAVE_XFPU_DP_FULL}            \
%{mfpu=*:   -DHAVE_XFPU}"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "\
%{!nostdlib: --start-group -lxil -lc -lm --end-group   \
%{mppcperflib: %{mfpu=*: -lppcstr405 -lgcc}            \
%{!mfpu=*: -lppcstr405 -lppcfp -lgcc}}                 \
%{!mppcperflib: -lgcc}}"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "\
ecrti.o%s %{pg: %{!mno-clearbss: xil-pgcrt0.o%s} \
%{mno-clearbss: xil-sim-pgcrt0.o%s}}            \
%{!pg: %{!mno-clearbss: xil-crt0.o%s}           \
%{mno-clearbss: xil-sim-crt0.o%s}} crtbegin.o%s"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "-T xilinx.ld%s"
