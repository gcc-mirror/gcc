/* Subroutines for the Rust front end for the RISC-V architecture.
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

/* Implement TARGET_RUST_CPU_INFO for RISC-V targets.  */

void riscv_rust_target_cpu_info(void) {
    if (TARGET_64BIT)
        rust_add_target_info("target_arch", "riscv64");
    else
        rust_add_target_info("target_arch", "riscv32");

    // names derived from rustc and llvm 
    if (TARGET_SAVE_RESTORE) 
        rust_add_target_info("target_feature", "save-restore");
    // TODO: ensure below variable works
    if (riscv_mrelax)
        rust_add_target_info("target_feature", "relax");
    if (TARGET_MUL)
        rust_add_target_info("target_feature", "m");
    if (TARGET_ATOMIC)
        rust_add_target_info("target_feature", "a");
    if (TARGET_HARD_FLOAT)
        rust_add_target_info("target_feature", "f");
    if (TARGET_DOUBLE_FLOAT)
        rust_add_target_info("target_feature", "d");
    if (TARGET_RVC)
        rust_add_target_info("target_feature", "c");
    if (TARGET_RVE)
        rust_add_target_info("target_feature", "e");
    // TODO: add features based on "B" and "V" extensions when gcc adds them
    // TODO: if gcc has it, add "no-rvc-hints" flag 
    // TODO: if gcc has it, add reserve-x1 -> reserve-x31 (user reserve registers)
    if (TARGET_64BIT)
        rust_add_target_info("target_feature", "64bit");
    /* TODO: maybe add gcc features with no llvm equivalent, e.g. align-data, riscv-attribute, 
     * explicit-relocs, strict-align, cmodel, small-data-limit, branch-cost, plt, abi, 
     * preferred-stack-boundary, fdiv, div */
}
