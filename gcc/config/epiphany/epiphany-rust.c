/* Subroutines for the Rust front end for the Adapteva Epiphany architecture.
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
#include "tm_p.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for Adapteva Epiphany targets.  */

void epiphany_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "epiphany");

    // llvm seems to have no support for sky (nor historical support), so names are made up by me
    // TODO: should the "no" dichotomy be preserved? probably not, but which should be chosen?
    if (TARGET_HALF_REG_FILE)
        rust_add_target_info("target_feature", "half-reg-file");
    if (TARGET_PREFER_SHORT_INSN_REGS)
        rust_add_target_info("target_feature", "prefer-short-insn-regs");
    // TODO: maybe have a "branch-cost" feature? doesn't really fit well with "define-only", though
    if (TARGET_CMOVE)
        rust_add_target_info("target_feature", "cmove");
    // TODO: maybe have a "nops" feature? doesn't really fit well with "define-only", though
    if (TARGET_SOFT_CMPSF)
        rust_add_target_info("target_feature", "soft-cmpsf");
    else
        rust_add_target_info("target_feature", "no-soft-cmpsf");
    // TODO: maybe have a "stack-offset" feature? doesn't really fit well with "define-only", though
    if (TARGET_ROUND_NEAREST)
        rust_add_target_info("target_feature", "round-nearest");
    else
        rust_add_target_info("target_feature", "no-round-nearest");
    if (TARGET_LONG_CALLS)
        rust_add_target_info("target_feature", "long-calls");
    if (TARGET_SHORT_CALLS)
        rust_add_target_info("target_feature", "short-calls");
    if (TARGET_SMALL16)
        rust_add_target_info("target_feature", "small16");
    // TODO: output mfp-mode somehow - "define-only" may work, but idk
    if (TARGET_SPLIT_LOHI)
        rust_add_target_info("target_feature", "split-lohi");
    else
        rust_add_target_info("target_feature", "no-split-lohi");
    if (TARGET_POST_INC)
        rust_add_target_info("target_feature", "postinc");
    else
        rust_add_target_info("target_feature", "no-postinc");
    if (TARGET_POST_MODIFY)
        rust_add_target_info("target_feature", "postmodify");
    else
        rust_add_target_info("target_feature", "no-postmodify");
    if (TARGET_VECT_DOUBLE)
        rust_add_target_info("target_feature", "vect-double");
    else
        rust_add_target_info("target_feature", "no-vect-double");
    // TODO: maybe have a "max-vect-align" feature? doesn't really fit well with "define-only", though
    if (TARGET_SPLIT_VECMOVE_EARLY)
        rust_add_target_info("target_feature", "split-vecmove-early");
    // TODO: maybe a feature about having the -1 register (1reg)?
    if (TARGET_FP_IARITH)
        rust_add_target_info("target_feature", "fp-iarith");
    if (TARGET_MAY_ROUND_FOR_TRUNC)
        rust_add_target_info("target_feature", "may-round-for-trunc");
}
