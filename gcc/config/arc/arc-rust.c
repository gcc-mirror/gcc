/* Subroutines for the Rust front end on the Synopsys DesignWare ARC cpu.
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

/* Implement TARGET_RUST_CPU_INFO for Synopsys DesignWare ARC targets.  */

void
arc_rust_target_cpu_info (void)
{
  rust_add_target_info ("target_arch", "arc");

  /* in llvm, the arc target has no "subtargets" (features according to rustc) as far as I can tell
   * gcc has a bunch of target macros that look like they could represent features, which I've added
   * provisionally. TODO add and rename features based on llvm. */
  if (TARGET_NORM)
    rust_add_target_info ("target_feature", "norm");
  if (TARGET_OPTFPE)
    rust_add_target_info ("target_feature", "optfpe");
  if (TARGET_SWAP)
    rust_add_target_info ("target_feature", "swap");

  if (TARGET_UNALIGN_BRANCH)
    rust_add_target_info ("target_feature", "unalign-branch");
  if (TARGET_PAD_RETURN)
    rust_add_target_info ("target_feature", "pad-return");
  if (TARGET_AT_DBR_CONDEXEC)
    rust_add_target_info ("target_feature", "at-dbr-condexec");

  // TODO: maybe define different cpu types? 

  // TODO: are all these below needed and useful?
  if (TARGET_MPYW)
    rust_add_target_info ("target_feature", "mpyw");
  if (TARGET_MULTI)
    rust_add_target_info ("target_feature", "multi");
  if (TARGET_MPY)
    rust_add_target_info ("target_feature", "mpy");
  if (TARGET_ARC700_MPY)
    rust_add_target_info ("target_feature", "arc700-mpy");
  if (TARGET_ANY_MPY)
    rust_add_target_info ("target_feature", "any-mpy");
  if (TARGET_PLUS_DMPY)
    rust_add_target_info ("target_feature", "plus-dmpy");
  if (TARGET_PLUS_MACD)
    rust_add_target_info ("target_feature", "plus-macd");
  if (TARGET_PLUS_QMACW)
    rust_add_target_info ("target_feature", "plus-qmacw");
  if (TARGET_LP_WR_INTERLOCK)
    rust_add_target_info ("target_feature", "lp-wr-interlock");

  // TODO: should different cpu families be removed?
  if (TARGET_ARC600_FAMILY)
    rust_add_target_info ("target_feature", "arc600-family");
  if (TARGET_ARCOMPACT_FAMILY)
    rust_add_target_info ("target_feature", "arcompact-family");

  if (TARGET_HARD_FLOAT)
    rust_add_target_info ("target_feature", "hard-float");
  if (TARGET_FP_SP_BASE)
    rust_add_target_info ("target_feature", "fp-sp-base");
  if (TARGET_FP_DP_BASE)
    rust_add_target_info ("target_feature", "fp-dp-base");
  if (TARGET_FP_SP_FUSED)
    rust_add_target_info ("target_feature", "fp-sp-fused");
  if (TARGET_FP_DP_FUSED)
    rust_add_target_info ("target_feature", "fp-dp-fused");
  if (TARGET_FP_SP_CONV)
    rust_add_target_info ("target_feature", "fp-sp-conv");
  if (TARGET_FP_DP_CONV)
    rust_add_target_info ("target_feature", "fp-dp-conv");
  if (TARGET_FP_SP_SQRT)
    rust_add_target_info ("target_feature", "fp-sp-sqrt");
  if (TARGET_FP_DP_SQRT)
    rust_add_target_info ("target_feature", "fp-dp-sqrt");
  if (TARGET_FP_DP_AX)
    rust_add_target_info ("target_feature", "fp-dp-ax");
  if (TARGET_FPX_QUARK)
    rust_add_target_info ("target_feature", "fpx-quark");
  if (TARGET_DBNZ)
    rust_add_target_info ("target_feature", "dbnz");
  
  if (TARGET_BI_BIH)
    rust_add_target_info ("target_feature", "bi-bih");
}
