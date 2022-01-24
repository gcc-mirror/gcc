/* Subroutines for the Rust front end for C-SKY targets.
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

/* Implement TARGET_RUST_CPU_INFO for C-SKY targets.  */

void cris_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "csky");

    // llvm seems to have no support for sky (nor historical support), so names are made up by me
    // TODO maybe put in sub-arches as features? idk. might be useful in this case 
    if (TARGET_HARD_FLOAT)
        rust_add_target_info("target_feature", "hard-float");
    else
        rust_add_target_info("target_feature", "soft-float");
    if (TARGET_DOUBLE_FLOAT)
        rust_add_target_info("target_feature", "double-float");
    if (TARGET_FDIVDU)
        rust_add_target_info("target_feature", "fdivdu");
    if (TARGET_ELRW)
        rust_add_target_info("target_feature", "elrw");
    if (TARGET_ISTACK)
        rust_add_target_info("target_feature", "istack");
    if (TARGET_MP)
        rust_add_target_info("target_feature", "mp");
    if (TARGET_CP)
        rust_add_target_info("target_feature", "cp");
    if (TARGET_CACHE)
        rust_add_target_info("target_feature", "cache");
    if (TARGET_SECURITY)
        rust_add_target_info("target_feature", "security"); // maybe also add define for "mac"?
    if (TARGET_TRUST)
        rust_add_target_info("target_feature", "trust");
    if (TARGET_DSP)
        rust_add_target_info("target_feature", "dsp");
    if (TARGET_EDSP)
        rust_add_target_info("target_feature", "edsp");
    if (TARGET_VDSP)
        rust_add_target_info("target_feature", "vdsp");
    if (TARGET_DIV)
        rust_add_target_info("target_feature", "div");
    if (TARGET_MINI_REGISTERS)
        rust_add_target_info("target_feature", "smart");
    if (TARGET_HIGH_REGISTERS)
        rust_add_target_info("target_feature", "high-registers");
    if (TARGET_ANCHOR)
        rust_add_target_info("target_feature", "anchor");
    if (TARGET_PUSHPOP)
        rust_add_target_info("target_feature", "pushpop");
    if (TARGET_MULTIPLE_STLD)
        rust_add_target_info("target_feature", "multiple-stld"); // maybe also add define for "stm"?
    if (TARGET_CONSTANT_POOL)
        rust_add_target_info("target_feature", "constpool");
    if (TARGET_STACK_SIZE)
        rust_add_target_info("target_feature", "stack-size");
    if (TARGET_LIBCCRT)
        rust_add_target_info("target_feature", "ccrt");
    // maybe have branch cost as target feature? but kind of doesn't really fit as "define"
    if (flag_sched_prolog)
        rust_add_target_info("target_feature", "sched-prolog");
}
