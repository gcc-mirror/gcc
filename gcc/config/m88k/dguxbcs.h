/* Definitions of target machine for GNU compiler.
   Motorola m88100 running DG/UX.
   Copyright (C) 1988, 1989, 1990, 1991 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)
   Enhanced by Michael Meissner (meissner@osf.org)
   Version 2 port by Tom Wood (twood@pets.sps.mot.com)
   Currently maintained by (gcc@dg-rtp.dg.com)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m88k/dgux.h"

/* dgux.h builds an elf gcc which compiles elf objects by default.
   dguxbcs.h builds a bcs gcc which compiles bcs objects by default.
   The default can be overridden in either case with -msvr3 and -msvr4 */

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV	 | \
			 MASK_OCS_DEBUG_INFO	 | \
			 MASK_OCS_FRAME_POSITION | \
			 MASK_SVR3)

/* Assembler support (-V, silicon filter, legends for mxdb).  */
#undef	ASM_SPEC
#define ASM_SPEC "\
%{V} %{v:%{!V:-V}} %{pipe:%{!.s: - }\
%{msvr4:%{!m88110:-KV3 }%{m88110:-KV04.00 }}}\
%{!mlegend:%{mstandard:-Wc,off}}\
%{mlegend:-Wc,-fix-bb,-h\"gcc-" VERSION_INFO2 "\",-s\"%i\"\
%{traditional:,-lc}%{!traditional:,-lansi-c}\
%{mstandard:,-keep-std}\
%{mkeep-coff:,-keep-coff}\
%{mexternal-legend:,-external}\
%{mocs-frame-position:,-ocs}}"

/* If -m88100 is in effect, add -Dm88100; similarly for -m88110.
   Here, the CPU_DEFAULT is assumed to be -m88000.  If not -ansi,
   -traditional, or restricting include files to one specific source
   target, specify full DG/UX features.  */
#undef	CPP_SPEC
#define	CPP_SPEC "%{!m88000:%{!m88100:%{m88110:-D__m88110__}}} \
		  %{!m88000:%{!m88110:%{m88100:-D__m88100__}}} \
		  %{!ansi:%{!traditional:-D__OPEN_NAMESPACE__}} \
		  %{!msvr4:-D_M88KBCS_TARGET} %{msvr4:-D_DGUX_TARGET}"

/* Linker and library spec's.
   -msvr3 is the default if -msvr4 is not specified. */
#undef	LIB_SPEC
#define LIB_SPEC "%{msvr4:%{!shared:-lstaticdgc}} %{!shared:%{!symbolic:-lc}} crtend.o%s"
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{!symbolic:%{pg:gcrt0.o%s} \
			 %{!pg:%{p:/lib/mcrt0.o}%{!p:/lib/crt0.o}} \
			 %{!msvr4:m88kdgux.ld%s bcscrtbegin.o%s} \
			 %{msvr4:crtbegin.o%s} \
			 %{svr4:%{ansi:/lib/values-Xc.o} \
			  %{!ansi:%{traditional:/lib/values-Xt.o} \
			   %{!traditional:/usr/lib/values-Xa.o}}}}}"
