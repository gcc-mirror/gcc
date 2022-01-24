/* Subroutines for the Rust front end for the OpenRISC architecture.
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

/* Implement TARGET_RUST_CPU_INFO for OpenRISC targets.  */

void or1k_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "or1k");

    // names derived from llvm fork
    if (!(TARGET_SOFT_MUL)) 
        rust_add_target_info("target_feature", "mul");
    if (!(TARGET_SOFT_DIV))
        rust_add_target_info("target_feature", "div");
    if (TARGET_ROR) 
        rust_add_target_info("target_feature", "ror");
    if (TARGET_CMOV)
        rust_add_target_info("target_feature", "cmov");
    /* TODO: add options for addc (add with carry), ffl1 (find first/last one), interrupts (use 
     * l.lwa/l.swa for atomic RMW ops) if can find gcc equivalents.  */
    if (TARGET_SEXT)
        rust_add_target_info("target_feature", "ext");

    // below are options not in llvm but derived from gcc, as they seemed potentially useful
    if (TARGET_HARD_FLOAT)
        rust_add_target_info("target_feature", "hard-float");
    if (TARGET_DOUBLE_FLOAT)
        rust_add_target_info("target_feature", "double-float");
    if (TARGET_FP_UNORDERED)
        rust_add_target_info("target_feature", "unordered-float");
    if (TARGET_RORI) 
        rust_add_target_info("target_feature", "rori");
    if (TARGET_SFIMM)
        rust_add_target_info("target_feature", "sfimm");
    if (TARGET_SHFTIMM) 
        rust_add_target_info("target_feature", "shftimm");
}
