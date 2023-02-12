/* Definitions of ELF target support for Altera Nios II.
   Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Jonah Graham (jgraham@altera.com), 
   Will Reece (wreece@altera.com), and Jeff DaSilva (jdasilva@altera.com).
   Contributed by Mentor Graphics, Inc.

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


/* Specs to support the additional command-line options for Nios II ELF
   toolchains.  */

/* -msmallc chooses an alternate C library.
   -msys-lib= specifies an additional low-level system/hosting library and
   is typically used to suck in a library provided by a HAL BSP.  */
#undef LIB_SPEC
#define LIB_SPEC \
"--start-group %{msmallc: -lsmallc} %{!msmallc: -lc} -lgcc \
 %{msys-lib=*: -l%*} \
 --end-group \
"

/* Linking with -mhal suppresses inclusion of the GCC-provided crt* begin/end
   code.  Normally in this case you also link with -msys-crt0= to specify
   the startup code provided by the HAL BSP instead.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC						\
  "%{mhal:"							\
  "%{msys-crt0=*:%*} %{!msys-crt0=*:crt0%O%s} "			\
  "%{msys-crt0=:%eYou need a C startup file for -msys-crt0=};"	\
  ":crti%O%s crtbegin%O%s}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%{!mhal:crtend%O%s crtn%O%s}"

/* The ELF target doesn't support the Nios II Linux ABI.  */
#define TARGET_LINUX_ABI 0

/* Default -fdelete-null-pointer-checks to off, to prevent the compiler
   from treating accesses to address zero as traps.  On bare-metal Nios II
   targets address zero may legitimately be mapped to memory (e.g., the
   hardware description may specify this as the address of the interrupt
   vector).  Users can override this on the command line to get the
   additional optimizations it enables.  */
#define SUBTARGET_OVERRIDE_OPTIONS 		\
  if (flag_delete_null_pointer_checks < 0)	\
    flag_delete_null_pointer_checks = 0
