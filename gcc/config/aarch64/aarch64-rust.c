/* Subroutines for the Rust front end on the AArch64 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for AArch64 targets.  */

void
aarch64_rust_target_cpu_info (void)
{
  rust_add_target_info ("target_arch", "aarch64");

  // TODO: properly change at some point instead of macro def
#ifndef isa_flag
# define isa_flag aarch64_isa_flags
# define isa_flag2 aarch64_isa_flags2
//# define fpmath aarch64_fpmath
#else
# error "isa_flag and isa_flag2 already defined in aarch64-rust.c - weird things might happen"
#endif
  // FIXME: almost feature-complete with rustc, missing "ras" feature (which may not be in gcc)

  if (TARGET_SIMD) 
    rust_add_target_info ("target_feature", "neon");
  /* appears to be what is referred to what seems to be referred to
  as "fp-armv8" in rust, at least in target def, based on closeness of it in aarch64.h to TARGET_SIMD */
  if (TARGET_FLOAT) {
    rust_add_target_info ("target_feature", "fp-armv8");
    // seems to be translated to "fp", but can't tell if "fp-armv8" exists too
    rust_add_target_info ("target_feature", "fp");
  }
  /*if (TARGET_CYCLONE) - need to find this feature (if it exists)
    rust_add_target_info ("target_feature", "cyclone");*/  
  /* appears to be what is referred to what seems to be referred to
  as "strict-align" in rust, at least in target def according to notes in aarch64.h 
  otherwise strict-align could be STRICT_ALIGNMENT (which evaluates to same thing unless macro redefed) */
  if (TARGET_STRICT_ALIGN) 
    rust_add_target_info ("target_feature", "strict-align");
  // below all derived from llvm code - i'm pretty sure they correspond
  if (TARGET_CRC32) 
    rust_add_target_info ("target_feature", "crc");
  if (TARGET_CRYPTO) 
    rust_add_target_info ("target_feature", "crypto");
  if (TARGET_DOTPROD) 
    rust_add_target_info ("target_feature", "dotprod");
  if (TARGET_F16FML) 
    rust_add_target_info ("target_feature", "fp16fml");
  if (TARGET_FP_F16INST) {
    rust_add_target_info ("target_feature", "fullfp16");
    // seems to be translated to "fp16", but not sure, so keep that here too
    rust_add_target_info ("target_feature", "fp16");
  }
  // TODO: some feature relating to profiling with feature name "spe" - can't find atm
  if (TARGET_LSE)
    rust_add_target_info ("target_feature", "lse");
  // hope this is the right thing - llvm calls it "rdm" - TODO ensure that it is
  if (AARCH64_ISA_RDMA)
    rust_add_target_info ("target_feature", "rdm");
  if (TARGET_SVE)
    rust_add_target_info ("target_feature", "sve");
  if (TARGET_SVE2)
    rust_add_target_info ("target_feature", "sve2");
  if (isa_flag & AARCH64_FL_SVE2_AES)
    rust_add_target_info ("target_feature", "sve2-aes");
  if (isa_flag & AARCH64_FL_SVE2_SM4)
    rust_add_target_info ("target_feature", "sve2-sm4");
  if (isa_flag & AARCH64_FL_SVE2_SHA3)
    rust_add_target_info ("target_feature", "sve2-sha3");
  if (isa_flag & AARCH64_FL_SVE2_BITPERM)
    rust_add_target_info ("target_feature", "sve2-bitperm");
  // TODO: assuming that this is the correct RCPC and that the AARCH64_FL_RCPC8_4 is not
  if (isa_flag & AARCH64_FL_RCPC)
    rust_add_target_info ("target_feature", "rcpc");
  // TODO: find below target features if they exist
  /*if (TARGET_ZCM)
    rust_add_target_info ("target_feature", "zcm");*/
  /*if (TARGET_ZCZ)
    rust_add_target_info ("target_feature", "zcz");*/
  // some possible target features: "thumb-mode"
  if (TARGET_SM4)
    rust_add_target_info ("target_feature", "sm4");
  if (TARGET_SHA3)
    rust_add_target_info ("target_feature", "sha3");
  if (TARGET_SHA2)
    rust_add_target_info ("target_feature", "sha2");
  if (TARGET_AES)
    rust_add_target_info ("target_feature", "aes");
  if (TARGET_TME)
    rust_add_target_info ("target_feature", "tme");
  if (TARGET_MEMTAG)
    rust_add_target_info ("target_feature", "mte");

  if (AARCH64_ISA_V8_1)
    rust_add_target_info ("target_feature", "v8.1a");
  if (AARCH64_ISA_V8_2)
    rust_add_target_info ("target_feature", "v8.2a");
  if (AARCH64_ISA_V8_3)
    rust_add_target_info ("target_feature", "v8.3a");
  if (AARCH64_ISA_V8_4)
    rust_add_target_info ("target_feature", "v8.4a");
  if (AARCH64_ISA_V8_5)
    rust_add_target_info ("target_feature", "v8.5a");
    
#undef isa_flag
#undef isa_flag2
//#undef fpmath
}
