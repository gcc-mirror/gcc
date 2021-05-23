/* Subroutines for the Rust front end on the AArch64 architecture.
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
#include "tm_p.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_CPU_INFO for AArch64 targets.  */

void aarch64_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "aarch64");

    // TODO: almost feature-complete with rustc - missing "ras" feature (which is not in gcc)

    // features from rustc
    if (TARGET_FLOAT) {
        rust_add_target_info("target_feature", "fp-armv8");
        rust_add_target_info("target_feature", "fp");
    }
    if (TARGET_SIMD)
        rust_add_target_info("target_feature", "neon");
    if (TARGET_SVE)
        rust_add_target_info("target_feature", "sve");
    if (TARGET_CRC32)
        rust_add_target_info("target_feature", "crc");
    if (TARGET_CRYPTO)
        rust_add_target_info("target_feature", "crypto");
    if (TARGET_LSE)
        rust_add_target_info("target_feature", "lse");
    if (AARCH64_ISA_RDMA)
        rust_add_target_info("target_feature", "rdm");
    if (TARGET_FP_F16INST) {
        rust_add_target_info("target_feature", "fullfp16");
        rust_add_target_info("target_feature", "fp16");
    }
    if (aarch64_isa_flags & AARCH64_FL_RCPC)
        rust_add_target_info("target_feature", "rcpc");
    if (TARGET_DOTPROD)
        rust_add_target_info("target_feature", "dotprod");
    if (aarch64_isa_flags & AARCH64_FL_V8_2)
        rust_add_target_info("target_feature", "v8.1a");
    if (AARCH64_ISA_V8_2)
        rust_add_target_info("target_feature", "v8.2a");
    if (AARCH64_ISA_V8_3)
        rust_add_target_info("target_feature", "v8.3a");

    // llvm-derived features
    if (TARGET_SM4)
        rust_add_target_info("target_feature", "sm4");
    if (TARGET_SHA2)
        rust_add_target_info("target_feature", "sha2");
    if (TARGET_SHA3)
        rust_add_target_info("target_feature", "sha3");
    if (TARGET_AES)
        rust_add_target_info("target_feature", "aes");
    if (TARGET_F16FML)
        rust_add_target_info("target_feature", "fp16fml");
    if (aarch64_isa_flags & AARCH64_FL_PROFILE)
        rust_add_target_info("target_feature", "spe");
    if (TARGET_SVE2)
        rust_add_target_info("target_feature", "sve2");
    if (aarch64_isa_flags & AARCH64_FL_SVE2_AES)
        rust_add_target_info("target_feature", "sve2-aes");
    if (aarch64_isa_flags & AARCH64_FL_SVE2_SM4)
        rust_add_target_info("target_feature", "sve2-sm4");
    if (aarch64_isa_flags & AARCH64_FL_SVE2_SHA3)
        rust_add_target_info("target_feature", "sve2-sha3");
    if (aarch64_isa_flags & AARCH64_FL_SVE2_BITPERM)
        rust_add_target_info("target_feature", "sve2-bitperm");
    if (TARGET_STRICT_ALIGN)
        rust_add_target_info("target_feature", "strict-align");
    if (flag_mrecip_low_precision_sqrt)
        rust_add_target_info("target_feature", "use-reciprocal-square-root");
    if (TARGET_JSCVT)
        rust_add_target_info("target_feature", "jsconv");
    if (TARGET_COMPLEX)
        rust_add_target_info("target_feature", "complxnum");
    if (AARCH64_ISA_RCPC8_4)
        rust_add_target_info("target_feature", "rcpc-immo");
    if (TARGET_FRINT)
        rust_add_target_info("target_feature", "fptoint");
    if (aarch64_isa_flags & AARCH64_FL_SB)
        rust_add_target_info("target_feature", "sb");
    if (aarch64_isa_flags & AARCH64_FL_SSBS)
        rust_add_target_info("target_feature", "ssbs");
    if (aarch64_isa_flags & AARCH64_FL_PREDRES)
        rust_add_target_info("target_feature", "predres");
    if (aarch64_enable_bti)
        rust_add_target_info("target_feature", "bti");
    if (AARCH64_ISA_RNG)
        rust_add_target_info("target_feature", "rand");
    if (TARGET_MEMTAG)
        rust_add_target_info("target_feature", "mte");
    if (TARGET_TME)
        rust_add_target_info("target_feature", "tme");
    if (AARCH64_ISA_BF16)
        rust_add_target_info("target_feature", "bf16");
    if (AARCH64_ISA_I8MM)
        rust_add_target_info("target_feature", "i8mm");
    if (AARCH64_ISA_F32MM)
        rust_add_target_info("target_feature", "f32mm");
    if (AARCH64_ISA_F64MM)
        rust_add_target_info("target_feature", "f64mm");
    if (AARCH64_ISA_V8_4)
        rust_add_target_info("target_feature", "v8.4a");
    if (AARCH64_ISA_V8_5)
        rust_add_target_info("target_feature", "v8.5a");
    if (AARCH64_ISA_V8_6)
        rust_add_target_info("target_feature", "v8.6a");

    /* TODO: find features for pan (Privileged Access-Never), lor (Limited Ordering Regions),
     * vh (Virtual Host), perfmon, pan-rwv (PAN s1e1R and s1e1W variants),
     * uaops (UAO PState), ccpp (Cache Clean to Point of Persistence), zcm (zero-cycle register move),
     * zcz-gp (zero-cycle zeroing for generic regs), zcz-fp (zero-cycle zeroing for FP regs), zcz
     * (both), zcz-fp-workaround, reserve specific registers, make specific registers callee saved,
     * use-aa (alias analysis), balance-fp-ops, predictable-select-expensive, custom-cheap-as-move,
     * exynos-cheap-as-move, use-postra-scheduler, slow-misaligned-128store, slow-paired-128,
     * slow-strqro-store, alternate-sextload-cvt-f32-pattern, arith-bcc-fusion, arith-cbz-fusion,
     * fuse-address, fuse-aes, fuse-arith-logic, fuse-csel, fuse-crypto-eor, fuse-literals,
     * disable-latency-sched-heuristic, force-32bit-jump-tables, pa
     * (Pointer Authentication), ccidx (extend CCSIDR number of sets),
     * nv (Nested Virtualisation), rasv8_4, mpam (Memory system Partitioning and Monitoring),
     * dit (Data Independent Timing), tracev8.4, am (Activity Monitors), amvs (Activity Monitors
     * Virtualisation), sel2 (Secure Exception Level 2), pmu, tlb-rmi (TLB Range and Maintenance), fmi
     * (Flag Manipulation), no-neg-immediates, lsl-fast, aggressive-fma,
     * altnzcv, specrestrict, ccdp, trbe, ete, tagged-globals, fgt, ecv, maybe cpus */
    // gcc supports pointer authentication, but i can only find builtins and no switch for it
}
