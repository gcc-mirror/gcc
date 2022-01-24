/* Subroutines for the Rust front end for the Motorola 680x0/ColdFire architecture.
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

/* Implement TARGET_RUST_CPU_INFO for Motorola 680x0/ColdFire targets.  */

void m68k_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "m68k");

    /* llvm has no current or historical support for m68k, and llvm forks and mrustc don't suggest any 
     * true target feature names, so I made up names and features */
    // TODO: maybe define subarches as features? probably needed, but not sure how well it interacts
    if (TARGET_ALIGN_INT)
        rust_add_target_info("target_feature", "align-int");
    if (TARGET_BITFIELD)
        rust_add_target_info("target_feature", "bitfield");
    if (TARGET_CF_HWDIV)
        rust_add_target_info("target_feature", "div");
    if (TARGET_HARD_FLOAT)
        rust_add_target_info("target_feature", "hard-float");
    else
        rust_add_target_info("target_feature", "soft-float");
    if (TARGET_ID_SHARED_LIBRARY)
        rust_add_target_info("target_feature", "id-shared-library");
    if (TARGET_LONG_JUMP_TABLE_OFFSETS)
        rust_add_target_info("target_feature", "long-jump-table-offsets");
    if (TARGET_RTD)
        rust_add_target_info("target_feature", "rtd");
    if (TARGET_SHORT)
        rust_add_target_info("target_feature", "short");
    if (TARGET_PCREL)
        rust_add_target_info("target_feature", "pcrel");
    if (TARGET_SEP_DATA)
        rust_add_target_info("target_feature", "sep-data");
    // TODO: see if can get information about shared-library-id
    if (TARGET_STRICT_ALIGNMENT)
        rust_add_target_info("target_feature", "strict-align");
    if (TARGET_XGOT)
        rust_add_target_info("target_feature", "xgot");   
    if (TARGET_XTLS)
        rust_add_target_info("target_feature", "xtls");
}
