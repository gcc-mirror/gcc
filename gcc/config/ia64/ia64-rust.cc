/* Subroutines for the Rust front end for the IA-64 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for IA-64 targets.  */

void ia64_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "ia64");

    // llvm does not appear to have defined features at any point for IA-64, so I made up names
    // TODO: should sub-arches be defined here?
    if (TARGET_GNU_AS)
        rust_add_target_info("target_feature", "gnu-as");
    if (TARGET_GNU_LD)
        rust_add_target_info("target_feature", "gnu-ld");
    if (TARGET_VOL_ASM_STOP)
        rust_add_target_info("target_feature", "volatile-asm-stop");
    if (TARGET_REG_NAMES)
        rust_add_target_info("target_feature", "register-names");
    if (TARGET_NO_SDATA)
        rust_add_target_info("target_feature", "no-sdata");
    else
        rust_add_target_info("target_feature", "sdata");
    if (TARGET_NO_PIC)
        rust_add_target_info("target_feature", "no-pic");
    if (TARGET_CONST_GP)
        rust_add_target_info("target_feature", "constant-gp");
    if (TARGET_AUTO_PIC)
        rust_add_target_info("target_feature", "auto-pic");
    
    switch (TARGET_INLINE_FLOAT_DIV) {
        case 0:
            rust_add_target_info("target_feature", "no-inline-float-divide");
            break;
        case 1:
            rust_add_target_info("target_feature", "inline-float-divide-min-latency");
            break;
        case 2:
            rust_add_target_info("target_feature", "inline-float-divide-max-throughput");
            break;
        default: // TODO: is this an error? should this be an error?
            break;
    }
    switch (TARGET_INLINE_INT_DIV) {
        case 0:
            rust_add_target_info("target_feature", "no-inline-int-divide");
            break;
        case 1:
            rust_add_target_info("target_feature", "inline-int-divide-min-latency");
            break;
        case 2:
            rust_add_target_info("target_feature", "inline-int-divide-max-throughput");
            break;
        default: // TODO: is this an error? should this be an error?
            break;
    }
    switch (TARGET_INLINE_SQRT) {
        case 0:
            rust_add_target_info("target_feature", "no-inline-sqrt");
            break;
        case 1:
            rust_add_target_info("target_feature", "inline-sqrt-min-latency");
            break;
        case 2:
            rust_add_target_info("target_feature", "inline-sqrt-max-throughput");
            break;
        default: // TODO: is this an error? should this be an error?
            break;
    }

    if (TARGET_DWARF2_ASM)
        rust_add_target_info("target_feature", "dwarf2-asm");
    if (TARGET_EARLY_STOP_BITS)
        rust_add_target_info("target_feature", "early-stop-bits");
    // TODO: do fixed-range somehow (wouldn't work well as define, I don't think), same for tls-size

    if (mflag_sched_br_data_spec)
        rust_add_target_info("target_feature", "sched-br-data-spec");
    if (mflag_sched_ar_data_spec)
        rust_add_target_info("target_feature", "sched-ar-data-spec");
    if (mflag_sched_control_spec)
        rust_add_target_info("target_feature", "sched-control-spec");
    if (mflag_sched_br_in_data_spec)
        rust_add_target_info("target_feature", "sched-br-in-data-spec");
    if (mflag_sched_ar_in_data_spec)
        rust_add_target_info("target_feature", "sched-ar-in-data-spec");
    if (mflag_sched_in_control_spec)
        rust_add_target_info("target_feature", "sched-in-control-spec");
    if (mflag_sched_spec_ldc)
        rust_add_target_info("target_feature", "sched-spec-ldc");
    if (mflag_sched_spec_control_ldc)
        rust_add_target_info("target_feature", "sched-spec-control-ldc");
    if (mflag_sched_count_spec_in_critical_path)
        rust_add_target_info("target_feature", "sched-count-spec-in-critical-path");
    if (mflag_sched_stop_bits_after_every_cycle)
        rust_add_target_info("target_feature", "sched-stop-bits-after-every-cycle");
    if (mflag_sched_fp_mem_deps_zero_cost)
        rust_add_target_info("target_feature", "sched-fp-mem-deps-zero-cost");
    if (mflag_sched_mem_insns_hard_limit)
        rust_add_target_info("target_feature", "sched-max-memory-insns-hard-limit");
    if (mflag_sel_sched_dont_check_control_spec)
        rust_add_target_info("target_feature", "sel-sched-dont-check-control-spec");
    // TODO: do sched-max-memory-insns somehow (wouldn't work well as define)
}
