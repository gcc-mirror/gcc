/* Subroutines for the Rust front end for the SPARC architecture.
   Copyright (C) 2020 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "tm_p.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for SPARC targets.  */

void sparc_rust_target_cpu_info(void) {
    if (TARGET_64BIT)
        rust_add_target_info("target_arch", "sparc64");
    else 
        rust_add_target_info("target_arch", "sparc");

    // names based on llvm 
    /* TODO: try to isolate soft-mul-div feature (software emulation for integer multiply and divide) 
     * if doable? does gcc even support this? */
    if (!(TARGET_FSMULD))
        rust_add_target_info("target_feature", "no-fsmuld");
    // TODO: add "no-fmuls" (fmuls instruction) option if can find in gcc
    if (TARGET_V9)
        rust_add_target_info("target_feature", "v9");
    if (TARGET_DEPRECATED_V8_INSNS)
        rust_add_target_info("target_feature", "deprecated-v8");
    if (TARGET_VIS)
        rust_add_target_info("target_feature", "vis");
    if (TARGET_VIS2)
        rust_add_target_info("target_feature", "vis2");
    if (TARGET_VIS3)
        rust_add_target_info("target_feature", "vis3");
    if (TARGET_LEON) // TODO: does this mean just leon or also allow leon v3?
        rust_add_target_info("target_feature", "leon");
    // TODO: add "leonpwrpsr" (PWRPSR instruction) option if can find in gcc
    if (TARGET_HARD_QUAD)
        rust_add_target_info("target_feature", "hard-quad-float");
    if (TARGET_POPC)
        rust_add_target_info("target_feature", "popc");
    if (!(TARGET_FPU))
        rust_add_target_info("target_feature", "soft-float");
    /* TODO: add "hasumacsmac" (UMAC and SMAC insns), "hasleoncasa" (CASA insns), 
     * "insertnopload" (LEON3 fix), "detectroundchange" (LEON3 fix), "fixallfdivsqrt" (LEON fix), 
     * "leoncyclecounter" if in gcc */

    // TODO: maybe add features in gcc that seem to have no llvm equivalent
}
