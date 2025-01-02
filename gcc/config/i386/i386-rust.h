/* Definitions for the Rust front end on the x86 architecture.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.

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

/* In i386-rust.cc  */
extern void ix86_rust_target_cpu_info (void);

/* Target CPU info for Rust.  */
#define TARGET_RUST_CPU_INFO ix86_rust_target_cpu_info
