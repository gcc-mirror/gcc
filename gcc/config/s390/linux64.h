/* Definitions for Linux for s/390 zSeries
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (weigand@de.ibm.com).
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

#ifndef _LINUX64_H
#define _LINUX64_H

#include <s390/linux.h>              /* Base linux target machine definitions*/

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT             0x13

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Linux for S/390 zSeries 64 bit)");

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dlinux -Asystem(linux) -Acpu(s390) -Amachine(s390) -D__s390x__ -Asystem(unix) -Dunix -D__ELF__"


#undef	LINK_SPEC
#ifdef CROSS_COMPILE
#define LINK_SPEC "-m elf64_s390 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld64.so.1 \
        -rpath-link=/usr/local/s390x-ibm-linux/lib}} \
	%{static:-static}}}"
#else
#define LINK_SPEC "-m elf64_s390 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld64.so.1}} \
	%{static:-static}}}"
#endif

#undef INT_ASM_OP
#define INT_ASM_OP "\t.quad\t"

#undef PROMOTE_PROTOTYPES 
#undef MASK_RETURN_ADDR 
#undef SELECT_SECTION

/* With 64 bit new linkage for floating point registers.  */
#undef CALL_USED_REGISTERS			
#define CALL_USED_REGISTERS			\
{ 1, 1, 1, 1, 					\
  1, 1, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 1, 1, 1,					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  1 }

#endif
