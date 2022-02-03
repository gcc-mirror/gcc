/* Subroutines for the Rust front end on the PowerPC architecture.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

/* Implement TARGET_RUST_CPU_INFO for PowerPC targets.  */

void
rs6000_rust_target_cpu_info (void)
{
  // note that rustc makes no arch distinction between powerpc64 and powerpc64 little endian
  if (TARGET_64BIT)
    rust_add_target_info ("target_arch", "powerpc64");
  else
    rust_add_target_info ("target_arch", "powerpc");

  // TODO: define properly instead of macros
#ifdef flags
# error "multiple flags already defined in rs6000-rust.cc"
#endif
#define flags rs6000_isa_flags

  // options should be (almost at least - i.e. power8-altivec and the like) feature complete with rustc
  if ((flags & OPTION_MASK_ALTIVEC) != 0)
    rust_add_target_info ("target_feature", "altivec");
  if ((flags & OPTION_MASK_VSX) != 0)
    rust_add_target_info ("target_feature", "vsx");
  /* I can't find any separate gcc equivalent to "power8-altivec" in llvm, but power8-vector has it as a
   * prerequisite, so just implicitly enable it when enabling the vector. TODO search for it. */
  if ((flags & OPTION_MASK_P8_VECTOR) != 0) {
    rust_add_target_info ("target_feature", "power8-vector");
    rust_add_target_info ("target_feature", "power8-altivec");
  }
  if ((flags & OPTION_MASK_CRYPTO) != 0)
    rust_add_target_info ("target_feature", "crypto");
  if ((flags & OPTION_MASK_HTM) != 0)
    rust_add_target_info ("target_feature", "htm");
  if ((flags & OPTION_MASK_FLOAT128_KEYWORD) != 0)
    rust_add_target_info ("target_feature", "float128");
  // Same implicit enabling of power9-altivec happens with power9-vector.
  if ((flags & OPTION_MASK_P9_VECTOR) != 0) {
    rust_add_target_info ("target_feature", "power9-vector");
    rust_add_target_info ("target_feature", "power9-altivec");
  }
  if ((flags & OPTION_MASK_DIRECT_MOVE) != 0)
    rust_add_target_info ("target_feature", "direct-move");

  if (TARGET_SECURE_PLT)
    rust_add_target_info ("target_feature", "secure-plt");

  if ((flags & OPTION_MASK_SOFT_FLOAT) != 0)
    ; // apparently not an option - TODO find out if it is
  else
    rust_add_target_info ("target_feature", "hard-float");

  // TODO: some possible features (in rustc, listed under powerpc-wrs-vxworks-spe) - "msync"
  // other possible features (in clang) - "qpx" (when cpu = "a2q"), "bpermd", "extdiv", "spe"

  // note: in gcc, it is possible bpermd is available if popcntd is available (which is power 7)

#undef flags
}
