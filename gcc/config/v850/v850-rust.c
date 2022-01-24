/* Subroutines for the Rust front end for the NEC V850 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for NEC V850 targets.  */

void v850_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "v850");

    // appears to have no current or historical support in llvm, so names made up by me
    if (TARGET_APP_REGS)
        rust_add_target_info("target_feature", "app-regs");
    if (TARGET_BIG_SWITCH) {
        rust_add_target_info("target_feature", "big-switch");
        rust_add_target_info("target_feature", "long-jumps");
    }
    if (TARGET_DEBUG)
        rust_add_target_info("target_feature", "debug");
    if (TARGET_DISABLE_CALLT)
        rust_add_target_info("target_feature", "disable-callt");
    if (TARGET_EP)
        rust_add_target_info("target_feature", "ep");
    if (TARGET_LONG_CALLS)
        rust_add_target_info("target_feature", "long-calls");
    if (TARGET_PROLOG_FUNCTION) 
        rust_add_target_info("target_feature", "prolog-function");
    // TODO: find if can get info from sda, zda and tda options
    if (TARGET_SMALL_SLD)
        rust_add_target_info("target_feature", "small-sld");
    if (TARGET_NO_STRICT_ALIGN)
        rust_add_target_info("target_feature", "no-strict-align");
    if (TARGET_JUMP_TABLES_IN_DATA_SECTION)
        rust_add_target_info("target_feature", "jump-tables-in-data-section");
    if (TARGET_US_BIT_SET)
        rust_add_target_info("target_feature", "US-bit-set");
    if (TARGET_V850)
        rust_add_target_info("target_feature", "v850");
    if (TARGET_V850E)
        rust_add_target_info("target_feature", "v850e");
    if (TARGET_V850E1)
        rust_add_target_info("target_feature", "v850e1");
    if (TARGET_V850E2)
        rust_add_target_info("target_feature", "v850e2");
    if (TARGET_V850E2V3)
        rust_add_target_info("target_feature", "v850e2v3");
    if (TARGET_V850E2V5) // TODO: how do i deal with V850E2V4 redirecting to this?
        rust_add_target_info("target_feature", "v850e2v5");
    if (TARGET_LOOP)
        rust_add_target_info("target_feature", "loop");
    if (TARGET_RELAX)
        rust_add_target_info("target_feature", "relax");
    if (TARGET_SOFT_FLOAT)
        rust_add_target_info("target_feature", "soft-float");
    else
        rust_add_target_info("target_feature", "hard-float");
    if (TARGET_GCC_ABI)
        rust_add_target_info("target_feature", "gcc-abi");
    else
        rust_add_target_info("target_feature", "rh850-abi");
    if (TARGET_8BYTE_ALIGN)
        rust_add_target_info("target_feature", "8byte-align");
}
