/* Subroutines for the Rust front end for the Renesas RX architecture.
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

/* Implement TARGET_RUST_CPU_INFO for RX targets.  */

void rx_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "rx");

    // llvm appears to have no (current or historical) support, so names made up by me
    if (TARGET_64BIT_DOUBLES) 
        rust_add_target_info("target_feature", "64bit-doubles");
    if (TARGET_NO_USE_FPU)
        rust_add_target_info("target_feature", "nofpu");
    // TODO: ensure below switch and variable works
    switch (rx_cpu_type) {
        case RX610:
            rust_add_target_info("target_feature", "cpu-rx610");
            break;
        case RX200:
            rust_add_target_info("target_feature", "cpu-rx200");
            break;
        case RX600:
            rust_add_target_info("target_feature", "cpu-rx600");
            break;
        case RX100:
            rust_add_target_info("target_feature", "cpu-rx100");
            break;
        default: // should this be an error? probably shouldn't happen
            break;
    }
    if (TARGET_BIG_ENDIAN_DATA)
        rust_add_target_info("target_feature", "big-endian-data");
    // TODO: find way of having small-data-limit, max-constant-size, int-register as defines
    // TODO: find way of getting info for relax
    if (TARGET_SAVE_ACC_REGISTER)
        rust_add_target_info("target_feature", "save-acc-in-interrupts");
    if (TARGET_PID)
        rust_add_target_info("target_feature", "pid");
    // TODO: ensure below variable works
    if (rx_warn_multiple_fast_interrupts)
        rust_add_target_info("target_feature", "warn-multiple-fast-interrupts");
    if (TARGET_GCC_ABI)
        rust_add_target_info("target_feature", "gcc-abi");
    else 
        rust_add_target_info("target_feature", "rx-abi");
    if (TARGET_ENABLE_LRA)
        rust_add_target_info("target_feature", "lra");
    // TODO: ensure below variable works
    if (rx_allow_string_insns)
        rust_add_target_info("target_feature", "allow-string-insns");
    if (TARGET_JSR)
        rust_add_target_info("target_feature", "jsr");
}
