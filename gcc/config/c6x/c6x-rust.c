/* Subroutines for the Rust front end on the TI C6X.
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

/* Implement TARGET_RUST_CPU_INFO for TI C6X targets.  */

void c6x_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "tic6x");

    // llvm seems to have no support for c6x (nor historical support), so names are made up by me
    // TODO maybe put in sub-arches as features? idk
    if (TARGET_DSBT)
        rust_add_target_info("target_feature", "dsbt");
    
    if (TARGET_INSNS_64)
        rust_add_target_info("target_feature", "c64x");
    if (TARGET_INSNS_64PLUS)
        rust_add_target_info("target_feature", "c64x+");
    if (TARGET_INSNS_67)
        rust_add_target_info("target_feature", "c67x");
    if (TARGET_INSNS_67PLUS)
        rust_add_target_info("target_feature", "c67x+");

    if (TARGET_LDDW)
        rust_add_target_info("target_feature", "lddw");
    if (TARGET_STDW)
        rust_add_target_info("target_feature", "stdw");
    if (TARGET_MPY32)
        rust_add_target_info("target_feature", "mpy32");
    if (TARGET_FP)
        rust_add_target_info("target_feature", "fp");
    if (TARGET_FP_EXT)
        rust_add_target_info("target_feature", "fp-ext");
}
