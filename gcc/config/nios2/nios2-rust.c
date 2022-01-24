/* Subroutines for the Rust front end for the Altera Nios II architecture.
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

/* Implement TARGET_RUST_CPU_INFO for Altera Nios II targets.  */

void nios2_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "nios2");

    // made up (most) names as only apparently basic historical support in llvm
    if (TARGET_HAS_DIV)
        rust_add_target_info("target_feature", "hw-div");
    if (TARGET_HAS_MUL)
        rust_add_target_info("target_feature", "hw-mul");
    if (TARGET_HAS_MULX)
        rust_add_target_info("target_feature", "hw-mulx");
    if (TARGET_FAST_SW_DIV)
        rust_add_target_info("target_feature", "fast-sw-div");
    if (TARGET_BYPASS_CACHE)
        rust_add_target_info("target_feature", "bypass-cache");
    if (TARGET_BYPASS_CACHE_VOLATILE)
        rust_add_target_info("target_feature", "no-cache-volatile");
    // TODO: ensure below switch variable and whatever works
    // TODO: improve how this works? the defining kinda sucks a bit
    switch (nios2_gpopt_option) {
        case gpopt_none:
            rust_add_target_info("target_feature", "gpopt-none");
            break;
        case gpopt_local:
            rust_add_target_info("target_feature", "gpopt-local");
            break;
        case gpopt_global:
            rust_add_target_info("target_feature", "gpopt-global");
            break;
        case gpopt_data:
            rust_add_target_info("target_feature", "gpopt-data");
            break;
        case gpopt_all:
            rust_add_target_info("target_feature", "gpopt-all");
            break;
        default: // unknown gpopt status - should this be an error?
            break;
    }
    if (TARGET_BIG_ENDIAN)
        rust_add_target_info("target_feature", "eb");
    else
        rust_add_target_info("target_feature", "el");
    /* TODO: figure out how to have custom-fpu-cfg, custom-ftruncds (including no-custom-ftruncds),
     * etc. (all custom instructions and their no- equivalents) in define form  */
    // TODO: ensure below switch and variable works
    switch (nios2_arch_option) {
        case ARCH_R1:
            rust_add_target_info("target_feature", "r1");
            rust_add_target_info("target_feature", "nios2r1");
            break;
        case ARCH_R2:
            rust_add_target_info("target_feature", "r2");
            rust_add_target_info("target_feature", "nios2r2");
            break;
        default: // should this be an error?
            break;
    }
    if (TARGET_HAS_BMX)
        rust_add_target_info("target_feature", "bmx");
    if (TARGET_HAS_CDX)
        rust_add_target_info("target_feature", "cdx");
    // TODO: figure out how to have gprel-sec and r0rel-sec as defines
    // TODO: maybe extra defines for features available on bare metal target? (hal, smallc, etc.)
}
