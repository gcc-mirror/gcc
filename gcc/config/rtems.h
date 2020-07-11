/* Configuration common to all targets running RTEMS. 
   Copyright (C) 2000-2020 Free Software Foundation, Inc.

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

#ifndef RTEMS_STARTFILE_SPEC
#define RTEMS_STARTFILE_SPEC "crti%O%s crtbegin%O%s"
#endif

#ifndef RTEMS_ENDFILE_SPEC
#define RTEMS_ENDFILE_SPEC "crtend%O%s crtn%O%s"
#endif

/*
 * The crt0.o is a dummy start file to let the linker work as needed by
 * autoconf scripts using this compiler.
 */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!qrtems:crt0%O%s} " \
"%{qrtems:%{!nostdlib:%{!nostartfiles:" RTEMS_STARTFILE_SPEC "}}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
"%{qrtems:%{!nostdlib:%{!nostartfiles:" RTEMS_ENDFILE_SPEC "}}}"

/*
 * Some targets do not set up LIB_SPECS, override it, here.
 */
#define STD_LIB_SPEC "%{!shared:%{g*:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

#undef LIB_SPEC
#define LIB_SPEC "%{!qrtems:" STD_LIB_SPEC "} " \
"%{qrtems:%{!nostdlib:%{!nodefaultlibs:" \
"--start-group -lrtemsbsp -lrtemscpu -latomic -lc -lgcc --end-group} " \
"%{!qnolinkcmds:-T linkcmds%s}}}"

#define TARGET_POSIX_IO

/* Prefer int for int32_t (see stdint-newlib.h).  */
#undef STDINT_LONG32
#define STDINT_LONG32 (INT_TYPE_SIZE != 32 && LONG_TYPE_SIZE == 32)
