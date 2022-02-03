/* Subroutines for the Rust front end for the PDP-11 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for PDP-11 targets.  */

void pdp11_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "pdp11");

    // names made up by me (as no apparent current nor historical llvm support), based on gcc options
    /* TODO: figure out how to get data for linker-opt, nosnake - not defined in variable, apparently */
    if (TARGET_AC0) 
        rust_add_target_info("target_feature", "ac0");
    if (TARGET_DEC_ASM)
        rust_add_target_info("target_feature", "dec-asm");
    if (TARGET_GNU_ASM)
        rust_add_target_info("target_feature", "gnu-asm");
    if (TARGET_UNIX_ASM) 
        rust_add_target_info("target_feature", "unix-asm");
    if (TARGET_FPU)
        rust_add_target_info("target_feature", "fpu");
    else
        rust_add_target_info("target_feature", "soft-float");
    if (TARGET_INT32)
        rust_add_target_info("target_feature", "int32");
    else
        rust_add_target_info("target_feature", "int16");
    if (TARGET_SPLIT)
        rust_add_target_info("target_feature", "split");
    if (TARGET_LRA)
        rust_add_target_info("target_feature", "lra");

    // defines for generating -40 and -45 code - TODO should -45 imply -40 as well? -10 seems implicit
    if (TARGET_40) 
        rust_add_target_info("target_feature", "pa-risc-1-1");
    if (TARGET_45) 
        rust_add_target_info("target_feature", "pa-risc-2-0");
}
