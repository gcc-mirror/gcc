/* Subroutines for the Rust front end for the NDS32 architecture.
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

/* Implement TARGET_RUST_CPU_INFO for NDS32 targets.  */

void nds32_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "nds32");

    // made up names as no apparent support (current or historical) in llvm
    // TODO: maybe have cpu types as features? might be a lot, though
    if (TARGET_REDUCED_REGS)
        rust_add_target_info("target_feature", "reduced-regs");
    if (TARGET_ALWAYS_ALIGN)
        rust_add_target_info("target_feature", "always-align");
    if (TARGET_ALIGN_FUNCTION)
        rust_add_target_info("target_feature", "align-functions");
    if (TARGET_FORCE_FP_AS_GP)
        rust_add_target_info("target_feature", "force-fp-as-gp");
    if (TARGET_FORBID_FP_AS_GP)
        rust_add_target_info("target_feature", "forbid-fp-as-gp");
    // TODO: ensure below variables work
    if (nds32_ict_model == ICT_MODEL_SMALL)
        rust_add_target_info("target_feature", "ict-model-small");
    else if (nds32_ict_model == ICT_MODEL_LARGE)
        rust_add_target_info("target_feature", "ict-model-large");
    if (TARGET_CMOV)
        rust_add_target_info("target_feature", "cmov");
    if (TARGET_HW_ABS)
        rust_add_target_info("target_feature", "hw-abs");
    if (TARGET_EXT_PERF)
        rust_add_target_info("target_feature", "ext-perf");
    if (TARGET_EXT_PERF2)
        rust_add_target_info("target_feature", "ext-perf2");
    if (TARGET_EXT_STRING)
        rust_add_target_info("target_feature", "ext-string");
    if (TARGET_EXT_DSP)
        rust_add_target_info("target_feature", "ext-dsp");
    if (TARGET_V3PUSH)
        rust_add_target_info("target_feature", "v3push");
    if (TARGET_16_BIT)
        rust_add_target_info("target_feature", "16-bit");
    if (TARGET_RELAX_HINT)
        rust_add_target_info("target_feature", "relax-hint");
    if (TARGET_VH)
        rust_add_target_info("target_feature", "vh");
    if (TARGET_ISR_VECTOR_SIZE_4_BYTE)
        rust_add_target_info("target_feature", "isr-vector-size-4");
    else
        rust_add_target_info("target_feature", "isr-vector-size-16");
    // TODO: figure out how to handle nds32_isr_secure_level (isr-secure)
    // TODO: ensure below switch variable and whatever works
    switch (nds32_cache_block_size) {
        // note: supposedly only powers of 2 between 4 and 512
        case 4:
            rust_add_target_info("target_feature", "cache-block-size-4");
            break;
        case 8:
            rust_add_target_info("target_feature", "cache-block-size-8");
            break;
        case 16:
            rust_add_target_info("target_feature", "cache-block-size-16");
            break;
        case 32:
            rust_add_target_info("target_feature", "cache-block-size-32");
            break;
        case 64:
            rust_add_target_info("target_feature", "cache-block-size-64");
            break;
        case 128:
            rust_add_target_info("target_feature", "cache-block-size-128");
            break;
        case 256:
            rust_add_target_info("target_feature", "cache-block-size-256");
            break;
        case 512:
            rust_add_target_info("target_feature", "cache-block-size-512");
            break;
        default: // unknown cache block size - should this be an error?
            break;
    }
    // TODO: ensure below switch and variable works - should this be cumulative or exclusive like now?
    switch (nds32_arch_option) {
        case ARCH_V2:
            rust_add_target_info("target_feature", "v2");
            break;
        case ARCH_V3:
            rust_add_target_info("target_feature", "v3");
            break;
        case ARCH_V3J:
            rust_add_target_info("target_feature", "v3j");
            break;
        case ARCH_V3M:
            rust_add_target_info("target_feature", "v3m");
            break;
        case ARCH_V3F:
            rust_add_target_info("target_feature", "v3f");
            break;
        case ARCH_V3S:
            rust_add_target_info("target_feature", "v3s");
            break;
        default: // unknown arch (isa level) - should this be an error?
            break;
    }
    // TODO: stuff below is taken from cpp defines - may be better to define 4-7 separately?
    if (TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE) {
        switch (nds32_fp_regnum) {
            case 0:
            case 4:
                rust_add_target_info("target_feature", "config-fpu-0");
                break;
            case 1:
            case 5:
                rust_add_target_info("target_feature", "config-fpu-1");
                break;
            case 2:
            case 6:
                rust_add_target_info("target_feature", "config-fpu-2");
                break;
            case 3:
            case 7:
                rust_add_target_info("target_feature", "config-fpu-3");
                break;
            default:
                gcc_unreachable();
        }
    }
    // TODO: ensure below switch and variable works
    switch (nds32_mul_config) {
        case MUL_TYPE_FAST_1:
            rust_add_target_info("target_feature", "config-mul-fast1");
            break;
        case MUL_TYPE_FAST_2:
            rust_add_target_info("target_feature", "config-mul-fast2");
            break;
        case MUL_TYPE_SLOW:
            rust_add_target_info("target_feature", "config-mul-slow");
            break;
        default: // unknown arch (isa level) - should this be an error?
            break;
    }
    // TODO: ensure below switch and variable works
    switch (nds32_register_ports_config) {
        case REG_PORT_3R2W:
            rust_add_target_info("target_feature", "config-register-ports-3r2w");
            break;
        case REG_PORT_2R1W:
            rust_add_target_info("target_feature", "config-register-ports-2r1w");
            break;
        default: // unknown arch (isa level) - should this be an error?
            break;
    }
    // TODO: add ctor-dtor and relax if can figure out how to get data from it
    if (TARGET_EXT_FPU_FMA)
        rust_add_target_info("target_feature", "ext-fpu-fma");
    if (TARGET_FPU_SINGLE)
        rust_add_target_info("target_feature", "ext-fpu-sp");
    if (TARGET_FPU_DOUBLE)
        rust_add_target_info("target_feature", "ext-fpu-dp");
    if (TARGET_FORCE_NO_EXT_DSP)
        rust_add_target_info("target_feature", "force-no-ext-dsp");
    // TODO: ensure below variables work
    if (flag_sched_prolog_epilog)
        rust_add_target_info("target_feature", "sched-prolog-epilog");
    if (flag_ret_in_naked_func)
        rust_add_target_info("target_feature", "ret-in-naked-func");
    if (flag_always_save_lp)
        rust_add_target_info("target_feature", "always-save-lp");
    if (flag_unaligned_access)
        rust_add_target_info("target_feature", "unaligned-access");
    if (flag_inline_asm_r15)
        rust_add_target_info("target_feature", "inline-asm-r15");
}
