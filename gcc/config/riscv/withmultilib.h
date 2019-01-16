/* MULTILIB_DEFAULTS definitions for --with-multilib-list.
   Copyright (C) 2018-2019 Free Software Foundation, Inc.

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

#if TARGET_MLIB_ARCH == 1

# if TARGET_MLIB_ABI == 1
#  define MULTILIB_DEFAULTS {"march=rv32gc", "mabi=ilp32" }
# elif TARGET_MLIB_ABI == 2
#  define MULTILIB_DEFAULTS {"march=rv32gc", "mabi=ilp32f" }
# elif TARGET_MLIB_ABI == 3
#  define MULTILIB_DEFAULTS {"march=rv32gc", "mabi=ilp32d" }
# else
#  error "unsupported TARGET_MLIB_ABI value for rv32gc"
# endif

#elif TARGET_MLIB_ARCH == 2

# if TARGET_MLIB_ABI == 4
#  define MULTILIB_DEFAULTS {"march=rv64gc", "mabi=lp64" }
# elif TARGET_MLIB_ABI == 5
#  define MULTILIB_DEFAULTS {"march=rv64gc", "mabi=lp64f" }
# elif TARGET_MLIB_ABI == 6
#  define MULTILIB_DEFAULTS {"march=rv64gc", "mabi=lp64d" }
# else
#  error "unsupported TARGET_MLIB_ABI value for rv64gc"
# endif

#else
# error "unsupported TARGET_MLIB_ARCH value"
#endif
