/* Definitions for the D front end on the AArch64 architecture.
   Copyright (C) 2022 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Defined in aarch64-d.cc  */
extern void aarch64_d_target_versions (void);
extern void aarch64_d_register_target_info (void);

/* Target hooks for D language.  */
#define TARGET_D_CPU_VERSIONS aarch64_d_target_versions
#define TARGET_D_REGISTER_CPU_TARGET_INFO aarch64_d_register_target_info
