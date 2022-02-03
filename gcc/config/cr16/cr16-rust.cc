/* Subroutines for the Rust front end on the CR16 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for CR16 targets.  */

void cr16_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "cr16");

    // llvm seems to have no support for cr16 (nor historical support), so names are made up by me
    // TODO maybe put in sub-arches as features? idk
    if (TARGET_BIT_OPS)
        rust_add_target_info("target_feature", "bit-ops");
    if (TARGET_MAC)
        rust_add_target_info("target_feature", "mac");
    if (TARGET_DEBUG_ADDR)
        rust_add_target_info("target_feature", "debug-addr");
    if (TARGET_INT32)
        rust_add_target_info("target_feature", "int32");

    if (CR16_TARGET_DATA_NEAR)             
        rust_add_target_info("target_feature", "data-model-near");
    if (CR16_TARGET_DATA_MEDIUM)           
        rust_add_target_info("target_feature", "data-model-medium");
    if (CR16_TARGET_DATA_FAR)              
        rust_add_target_info("target_feature", "data-model-far");  
    
    if (TARGET_CR16C)
        rust_add_target_info("target_feature", "cr16c");
    if (TARGET_CR16CP)
        rust_add_target_info("target_feature", "cr16cplus");
}
