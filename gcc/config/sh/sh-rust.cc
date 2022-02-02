/* Subroutines for the Rust front end for the Renesas / SuperH SH architecture.
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

/* Implement TARGET_RUST_CPU_INFO for Renesas / SuperH SH targets.  */

void sh_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "sh");

    // llvm appears to have no (current or historical) support, so names made up by me
    // TODO: should arch be cumulative or exclusive? if cumulative, what is dependency of stuff like "e"?
    // TODO: maybe distinguish between just "sh4" and stuff like "sh4-200" and "sh4-300"?
    switch ((int)sh_cpu) {
        case PROCESSOR_SH1:
            rust_add_target_info("target_feature", "sh1");
            break;
        case PROCESSOR_SH2:
            rust_add_target_info("target_feature", "sh2");
            break;
        case PROCESSOR_SH2E:
            rust_add_target_info("target_feature", "sh2e");
            break;
        case PROCESSOR_SH2A:
            rust_add_target_info("target_feature", "sh2a");
            if (TARGET_SH2A_DOUBLE) {
                if (TARGET_FPU_SINGLE)
                    rust_add_target_info("target_feature", "single");
            } else {
                if (TARGET_FPU_ANY)
                    rust_add_target_info("target_feature", "single-only");
                else
                    rust_add_target_info("target_feature", "nofpu");
            }
            break;
        case PROCESSOR_SH3:      
            if (TARGET_HARD_SH4) {
                rust_add_target_info("target_feature", "sh4");
                rust_add_target_info("target_feature", "nofpu");
            } else {
                rust_add_target_info("target_feature", "sh3");
            }
            break;
        case PROCESSOR_SH3E:
            if (TARGET_HARD_SH4) {
                rust_add_target_info("target_feature", "sh4");
                rust_add_target_info("target_feature", "single-only");
            } else {
                rust_add_target_info("target_feature", "sh3e");
            }
            break;
        case PROCESSOR_SH4:
            rust_add_target_info("target_feature", "sh4");
            if (TARGET_FPU_SINGLE)
                rust_add_target_info("target_feature", "single");
            break;
        case PROCESSOR_SH4A:
            rust_add_target_info("target_feature", "sh4a");
            if (TARGET_SH4) {
                if (TARGET_FPU_SINGLE)
                    rust_add_target_info("target_feature", "single");
            } else {
                if (TARGET_FPU_ANY)
                    rust_add_target_info("target_feature", "single-only");
                else
                    rust_add_target_info("target_feature", "nofpu");
            }
            break;
        default: // should this be an error?
            break;
    }

    if (TARGET_ACCUMULATE_OUTGOING_ARGS)
        rust_add_target_info("target_feature", "accumulate-outgoing-args");
    if (TARGET_LITTLE_ENDIAN)
        rust_add_target_info("target_feature", "l");
    else
        rust_add_target_info("target_feature", "b");
    if (TARGET_BIGTABLE)
        rust_add_target_info("target_feature", "bigtable");
    if (TARGET_BITOPS)
        rust_add_target_info("target_feature", "bitops");
    // TODO: determine way of having branch-cost, fixed-range, multcost, divsi3_libfunc as defines
    if (TARGET_ZDCBRANCH)
        rust_add_target_info("target_feature", "zdcbranch");
    if (TARGET_CBRANCH_FORCE_DELAY_SLOT)
        rust_add_target_info("target_feature", "cbranch-force-delay-slot");
    if (TARGET_ALIGN_DOUBLE)
        rust_add_target_info("target_feature", "dalign");
    if (TARGET_DIVIDE_CALL_DIV1)
        rust_add_target_info("target_feature", "div-call-div1");
    else if (TARGET_DIVIDE_CALL_FP)
        rust_add_target_info("target_feature", "div-call-fp");
    else if (TARGET_DIVIDE_CALL_TABLE)
        rust_add_target_info("target_feature", "div-call-table");
    if (TARGET_FDPIC)
        rust_add_target_info("target_feature", "fdpic");
    if (TARGET_FMOVD)
        rust_add_target_info("target_feature", "fmovd");
    if (TARGET_HITACHI) {
        rust_add_target_info("target_feature", "hitachi");
        rust_add_target_info("target_feature", "renesas");
    }
    if (TARGET_IEEE)
        rust_add_target_info("target_feature", "ieee");
    if (TARGET_INLINE_IC_INVALIDATE)
        rust_add_target_info("target_feature", "inline-ic_invalidate");
    if (TARGET_DUMPISIZE)
        rust_add_target_info("target_feature", "isize");
    if (TARGET_NOMACSAVE)
        rust_add_target_info("target_feature", "nomacsave");
    // ignoring padstruct as set to be removed
    if (TARGET_PREFERGOT)
        rust_add_target_info("target_feature", "prefergot");
    if (TARGET_RELAX)
        rust_add_target_info("target_feature", "relax");
    if (TARGET_ENABLE_TAS)
        rust_add_target_info("target_feature", "tas");
    if (TARGET_USERMODE)
        rust_add_target_info("target_feature", "usermode");
    if (TARGET_PRETEND_CMOVE)
        rust_add_target_info("target_feature", "pretend-cmove");
    if (TARGET_FSCA)
        rust_add_target_info("target_feature", "fsca");
    if (TARGET_FSRRA)
        rust_add_target_info("target_feature", "fsrra");
    if (sh_lra_flag)
        rust_add_target_info("target_feature", "lra");

    if (selected_atomic_model().type == sh_atomic_model::none)
        rust_add_target_info("target_feature", "atomic-model-none");
    if (TARGET_ATOMIC_SOFT_GUSA)
        rust_add_target_info("target_feature", "atomic-model-soft-gusa");
    if (TARGET_ATOMIC_SOFT_TCB)
        rust_add_target_info("target_feature", "atomic-model-soft-tcb");
    if (TARGET_ATOMIC_SOFT_IMASK)
        rust_add_target_info("target_feature", "atomic-model-soft-imask");
    if (TARGET_ATOMIC_HARD_LLCS)
        rust_add_target_info("target_feature", "atomic-model-hard-llcs");
    if (TARGET_ATOMIC_STRICT)
        rust_add_target_info("target_feature", "atomic-model-strict");
    // TODO: maybe have gbr-offset (from atomic-model) as define?
}
