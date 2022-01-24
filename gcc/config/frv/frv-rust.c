/* Subroutines for the Rust front end for the FRV architecture.
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

/* Implement TARGET_RUST_CPU_INFO for FRV targets.  */

void frv_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "frv");

    // llvm seems to have no support for sky (nor historical support), so names are made up by me
    // TODO maybe put in sub-arches as features? idk. might be useful in this case 
    if (TARGET_ACC_4)
        rust_add_target_info("target_feature", "acc-4");
    if (TARGET_ACC_8)
        rust_add_target_info("target_feature", "acc-8");
    if (TARGET_ALIGN_LABELS)
        rust_add_target_info("target_feature", "align-labels");
    if (TARGET_ALLOC_CC)
        rust_add_target_info("target_feature", "alloc-cc");
    // TODO: maybe option on branch cost, but wouldn't work well as "define only" option
    if (TARGET_COND_EXEC)
        rust_add_target_info("target_feature", "cond-exec");
    /* TODO: maybe option on cond-exec-insns, but wouldn't work well as "define-only" option. 
     * also cond-exec-temps, sched-lookahead */
    if (TARGET_COND_MOVE)
        rust_add_target_info("target_feature", "cond-move");
    if (TARGET_DEBUG)
        rust_add_target_info("target_feature", "debug");
    if (TARGET_DEBUG_ARG)
        rust_add_target_info("target_feature", "debug-arg");
    if (TARGET_DEBUG_ADDR)
        rust_add_target_info("target_feature", "debug-addr");
    if (TARGET_DEBUG_COND_EXEC)
        rust_add_target_info("target_feature", "debug-cond-exec");
    if (TARGET_DEBUG_LOC)
        rust_add_target_info("target_feature", "debug-loc");
    if (TARGET_DEBUG_STACK)
        rust_add_target_info("target_feature", "debug-stack");
    if (TARGET_DOUBLE)
        rust_add_target_info("target_feature", "double");
    if (TARGET_DWORD)
        rust_add_target_info("target_feature", "dword");
    if (TARGET_FDPIC)
        rust_add_target_info("target_feature", "fdpic");
    if (TARGET_FIXED_CC)
        rust_add_target_info("target_feature", "fixed-cc");
    if (TARGET_FPR_32)
        rust_add_target_info("target_feature", "fpr-32");
    if (TARGET_FPR_64)
        rust_add_target_info("target_feature", "fpr-64");
    if (TARGET_GPR_32)
        rust_add_target_info("target_feature", "gpr-32");
    if (TARGET_GPR_64)
        rust_add_target_info("target_feature", "gpr-64");
    if (TARGET_GPREL_RO)
        rust_add_target_info("target_feature", "gprel-ro");
    if (TARGET_HARD_FLOAT)
        rust_add_target_info("target_feature", "hard-float");
    else
        rust_add_target_info("target_feature", "soft-float");
    if (TARGET_INLINE_PLT)
        rust_add_target_info("target_feature", "inline-plt");
    if (TARGET_LIBPIC)
        rust_add_target_info("target_feature", "library-pic");
    if (TARGET_LINKED_FP)
        rust_add_target_info("target_feature", "linked-fp");
    if (TARGET_LONG_CALLS)
        rust_add_target_info("target_feature", "long-calls");
    if (TARGET_MEDIA)
        rust_add_target_info("target_feature", "media");
    if (TARGET_MULADD)
        rust_add_target_info("target_feature", "muladd");
    if (TARGET_MULTI_CE)
        rust_add_target_info("target_feature", "multi-cond-exec");
    if (TARGET_NESTED_CE)
        rust_add_target_info("target_feature", "nested-cond-exec");
    // TODO: maybe something with no-eflags if possible? tomcat-stats? multilib-library-pic?
    if (TARGET_OPTIMIZE_MEMBAR)
        rust_add_target_info("target_feature", "optimize-membar");
    if (TARGET_PACK)
        rust_add_target_info("target_feature", "pack");
    if (TARGET_SCC)
        rust_add_target_info("target_feature", "scc");
    if (TARGET_BIG_TLS)
        rust_add_target_info("target_feature", "large-tls");
    else
        rust_add_target_info("target_feature", "small-tls");
    if (TARGET_VLIW_BRANCH)
        rust_add_target_info("target_feature", "vliw-branch");
}
