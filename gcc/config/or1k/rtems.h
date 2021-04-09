/* Target Newlib Definitions for OpenRISC.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel.sherrill@OARcorp.com).

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

/* Target OS builtins.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__rtems__");		\
	builtin_define ("__USE_INIT_FINI__");	\
	builtin_assert ("system=rtems");	\
    }						\
  while (0)

#ifdef TARGET_RUST_OS_INFO
# error "TARGET_RUST_OS_INFO already defined in rtems.h (or1k) - c++ undefines it and redefines it."
#endif
#define TARGET_RUST_OS_INFO()		\
  do {						\
    /*note: as far as I know, rustc has no supported for rtems, so this is just guessed*/ \
    /*everything is subject to change, especially target_env and target_family*/ \
    builtin_rust_info ("target_family", "unix");			\
    builtin_rust_info ("target_os", "rtems");			\
    builtin_rust_info ("target_vendor", "unknown");			\
    builtin_rust_info ("target_env", "");			\
  } while (0)

#define RTEMS_STARTFILE_SPEC "crtbegin%O%s"
#define RTEMS_ENDFILE_SPEC "crtend%O%s"

