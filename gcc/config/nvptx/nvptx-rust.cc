/* Subroutines for the Rust front end for the Nvidia PTX architecture.
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

/* Implement TARGET_RUST_CPU_INFO for NVPTX targets.  */

void nvptx_rust_target_cpu_info(void) {
    if (TARGET_ABI64)
        rust_add_target_info("target_arch", "nvptx64");
    else
        rust_add_target_info("target_arch", "nvptx");

    // TODO: should this also override target_os and target_vendor to be "cuda" and "nvidia"?

    // names derived from llvm
    // TODO: ensure below variable and switch works
    switch (ptx_isa_option) {
        /* TODO: if gcc adds other sm versions (llvm has 20, 21, 32, 37, 50, 52, 53, 60, 61, 62, 70, 72, 
         * 75, 80 as well), add them here  */
        case PTX_ISA_SM30:
            rust_add_target_info("target_feature", "sm_30");
            break;
        case PTX_ISA_SM35:
            rust_add_target_info("target_feature", "sm_35");
            break;
        default: // should this be an error?
            break;
    }
    /* TODO: add ptx versions as separate features if gcc adds them (ptx32, 40, 41, 42, 43, 50, 60, 61, 
     * 63, 64, 65, 70)  */

    // NOTE: below are all gcc-derived features that do not appear in llvm. they appeared useful, so added
    // TODO: ensure below variable works
    if (nvptx_optimize)
        rust_add_target_info("target_feature", "optimize");
    if (TARGET_SOFT_STACK)
        rust_add_target_info("target_feature", "soft-stack");
    // TODO: find way to have soft-stack-reserve-local as define
    if (TARGET_UNIFORM_SIMT)
        rust_add_target_info("target_feature", "uniform-simt");
    if (TARGET_GOMP)
        rust_add_target_info("target_feature", "gomp");
}
