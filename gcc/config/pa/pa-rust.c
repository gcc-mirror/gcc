/* Subroutines for the Rust front end for the HPPA architecture.
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

/* Implement TARGET_RUST_CPU_INFO for HPPA targets.  */

void pa_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "hppa");

    // names made up by me (as no apparent current nor historical llvm support), based on gcc options
    /* TODO: figure out how to get data for linker-opt, nosnake - not defined in variable, apparently */
    if (TARGET_CALLER_COPIES) 
        rust_add_target_info("target_feature", "caller-copies");
    if (TARGET_COHERENT_LDCW)
        rust_add_target_info("target_feature", "coherent-ldcw");
    if (TARGET_DISABLE_FPREGS) 
        rust_add_target_info("target_feature", "disable-fpregs");
    if (TARGET_DISABLE_INDEXING)
        rust_add_target_info("target_feature", "disable-indexing");
    if (TARGET_FAST_INDIRECT_CALLS)
        rust_add_target_info("target_feature", "fast-indirect-calls");
    // TODO: figure out how to represent fixed-range (ranges of registers to make fixed) as define
    if (TARGET_GAS)
        rust_add_target_info("target_feature", "gas");
    if (TARGET_LONG_CALLS)
        rust_add_target_info("target_feature", "long-calls");
    if (TARGET_LONG_LOAD_STORE)
        rust_add_target_info("target_feature", "long-load-store");
    if (TARGET_NO_SPACE_REGS) 
        rust_add_target_info("target_feature", "no-space-regs");
    if (TARGET_ORDERED)
        rust_add_target_info("target_feature", "ordered");
    if (TARGET_PORTABLE_RUNTIME)
        rust_add_target_info("target_feature", "portable-runtime");
    if (TARGET_SOFT_FLOAT)
        rust_add_target_info("target_feature", "soft-float");

    // defines for generating PA 1.1 or PA 2.0 code - TODO should PA 2.0 imply PA 1.1 as well?
    if (TARGET_PA_11) 
        rust_add_target_info("target_feature", "pa-risc-1-1");
    if (TARGET_PA_20) 
        rust_add_target_info("target_feature", "pa-risc-2-0");

    // TODO: ensure switch and variable work
    switch (pa_cpu) {
        case PROCESSOR_8000:
            rust_add_target_info("target_feature", "schedule-8000");
            break;
        case PROCESSOR_7100:
            rust_add_target_info("target_feature", "schedule-7100");
            break;
        case PROCESSOR_700:
            rust_add_target_info("target_feature", "schedule-700");
            break;
        case PROCESSOR_7100LC:
            rust_add_target_info("target_feature", "schedule-7100lc");
            break;
        case PROCESSOR_7200:
            rust_add_target_info("target_feature", "schedule-7200");
            break;
        case PROCESSOR_7300:
            rust_add_target_info("target_feature", "schedule-7300");
            break;
        default: // should this be an error?
            break;
    }
}
