/* Definitions of target machine for GNU compiler.  Vxworks i960 version.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

/* This file just exists to give specs for the 960 running on VxWorks.  */

#include "i960/i960-coff.h"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

/* Predefine vxworks.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Di960 -Di80960 -DI960 -DI80960 -Dvxworks -Acpu(i960) -Amachine(i960)"

/* The VxWorks header files expect the compiler to define CPU to a
   magic number.  */

#undef CPP_SPEC
#define	CPP_SPEC "%{mic*:-D__i960\
			%{mka:-D__i960KA}%{mkb:-D__i960KB}\
			%{msa:-D__i960SA}%{msb:-D__i960SB}\
			%{mmc:-D__i960MC}\
			%{mca:-D__i960CA}%{mcc:-D__i960CC}\
			%{mcf:-D__i960CF}}\
	%{mka:-D__i960KA__ -D__i960_KA__ %{!ansi:-DCPU=I960KA}}\
	%{mkb:-D__i960KB__ -D__i960_KB__ %{!ansi:-DCPU=I960KB}}\
	%{msa:-D__i960SA__ -D__i960_SA__}\
	%{msb:-D__i960SB__ -D__i960_SB__}\
	%{mmc:-D__i960MC__ -D__i960_MC__}\
	%{mca:-D__i960CA__ -D__i960_CA__ %{!ansi:-DCPU=I960CA}}\
	%{mcc:-D__i960CC__ -D__i960_CC__}\
	%{mcf:-D__i960CF__ -D__i960_CF__}\
	%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:\
		%{!mcc:%{!mcf:-D__i960_CA -D__i960CA__ %{!ansi:-DCPU=I960CA}\
			      %{mic*:-D__i960CA}}}}}}}}}"

/* Default to -mca.  */

#undef CC1_SPEC
#define CC1_SPEC \
	"%{!mka:%{!mkb:%{!msa:%{!msb:%{!mmc:%{!mca:%{!mcc:%{!mcf:-mca}}}}}}}}\
	 %{!gs*:%{!gc*:%{mbout:%{g*:-gstabs}}\
		       %{mcoff:%{g*:-gcoff}}\
		       %{!mbout:%{!mcoff:%{g*:-gcoff}}}}}"
