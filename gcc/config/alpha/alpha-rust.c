/* Subroutines for the Rust front end on the DEC Alpha.
   Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for DEC Alpha targets.  */

void
alpha_rust_target_cpu_info (void)
{
  /* i couldn't actually confirm that this was the arch name (removed from llvm and no rustc support),
   * but i don't think they would choose something different to gcc */
  rust_add_target_info ("target_arch", "alpha");

  // CIX was actually the only llvm option available when it was removed, but adding other gcc ones
  if (TARGET_CIX)	
    rust_add_target_info ("target_feature", "cix");	
  if (TARGET_FIX)	
    rust_add_target_info ("target_feature", "fix");	
  if (TARGET_BWX)	
    rust_add_target_info ("target_feature", "bwx");	
  // may be called "mvi" under rustc (but they have no support for it atm, so who cares amirite?)
  if (TARGET_MAX)	
    rust_add_target_info ("target_feature", "max");	
}
