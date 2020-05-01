/* Subroutines for the Rust front end on the ARM architecture.
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

/* Implement TARGET_RUST_CPU_INFO for ARM targets.  */

void arm_rust_target_cpu_info(void) {
    rust_add_target_info("target_arch", "arm");

    /* TODO: further research support for CLREX, acquire-release (lda/ldaex), slow-fp-brcc (slow FP
     * compare and branch), perfmon, trustzone, fpao, fuse-aes, fuse-literals, read-tp-hard, zcz,
     * prof-unpr, slow-vgetlni32, slow-vdup32, prefer-vmovsr, prefer-ishst, muxed-units, slow-odd-reg,
     * slow-load-D-subreg, wide-stride-vfp, dont-widen-vmovs, splat-vfp-neon, expand-fp-mlx,
     * vmlx-hazards, neon-fpmovs, neonfp (as in using neon for scalar fp), vldn-align,
     * nonpipelined-vfp, slowfpvmlx, slowfpvfmx, vmlx-forwarding, 32bit (prefer 32-bit Thumb),
     * loop-align, mve1beat, mve2beat, mve4beat, avoid-partial-cpsr, cheap-predictable-cpsr,
     * avoid-movs-shop, ret-addr-stack, no-branch-predictor, virtualization, nacl-trap, execute-only,
     * reserve-r9, no-movt, no-neg-immediates, use-misched, disable-postra-scheduler, lob (Low
     * Overhead Branch), noarm, cde - can't find them. */
    /* TODO: figure out if gcc has an equivalent to "fpregs" (floating-point registers even if only
     * used for integer - shared between VFP and MVE).  */
    if (TARGET_VFPD32)
        rust_add_target_info("target_feature", "d32");
    bool hasFeatureVFP2 = bitmap_bit_p(arm_active_target.isa, isa_bit_vfpv2) && TARGET_VFP_DOUBLE;
    if (hasFeatureVFP2) {
        rust_add_target_info("target_feature", "vfp2");

        // also added implied features that aren't separately supported in gcc
        rust_add_target_info("target_feature", "vfp2sp");
    }
    // minimal VFPv3 support - support for instruction set, not necessarily full
    bool minVFP3 = TARGET_VFP3 && bitmap_bit_p(arm_active_target.isa, isa_bit_vfpv2);
    if (minVFP3) {
        rust_add_target_info("target_feature", "vfp3d16sp");

        if (TARGET_VFPD32)
            rust_add_target_info("target_feature", "vfp3sp");

        if (TARGET_VFP_DOUBLE) {
            rust_add_target_info("target_feature", "vfp3d16");

            if (TARGET_VFPD32) {
                rust_add_target_info("target_feature", "vfp3");

                if (bitmap_bit_p(arm_active_target.isa, isa_bit_neon))
                    rust_add_target_info("target_feature", "neon");
            }
        }
    }
    bool hasFeatureVFP3 = minVFP3 && TARGET_VFP_DOUBLE && TARGET_VFPD32;
    bool hasFeatureFP16 = bitmap_bit_p(arm_active_target.isa, isa_bit_fp16conv);
    if (hasFeatureFP16)
        rust_add_target_info("target_info", "fp16");
    bool minVFP4 = minVFP3 && bitmap_bit_p(arm_active_target.isa, isa_bit_vfpv4) && hasFeatureFP16;
    if (minVFP4) {
        rust_add_target_info("target_feature", "vfp4d16sp");

        if (TARGET_VFPD32)
            rust_add_target_info("target_feature", "vfp4sp");

        if (TARGET_VFP_DOUBLE) {
            rust_add_target_info("target_feature", "vfp4d16");

            if (TARGET_VFPD32) {
                rust_add_target_info("target_feature", "vfp4");
            }
        }
    }
    // NOTE: supposedly "fp-armv8" features in llvm are the same as "fpv5", so creating them based on
    // that
    bool minFP_ARMv8 = minVFP4 && TARGET_VFP5;
    if (minFP_ARMv8) {
        rust_add_target_info("target_feature", "fp-armv8d16sp");

        if (TARGET_VFPD32)
            rust_add_target_info("target_feature", "fp-armv8sp");

        if (TARGET_VFP_DOUBLE) {
            rust_add_target_info("target_feature", "fp-armv8d16");

            if (TARGET_VFPD32) {
                rust_add_target_info("target_feature", "fp-armv8");
            }
        }

        if (bitmap_bit_p(arm_active_target.isa, isa_bit_fp16)) {
            rust_add_target_info("target_feature", "fullfp16");

            if (bitmap_bit_p(arm_active_target.isa, isa_bit_fp16fml))
                rust_add_target_info("target_feature", "fp16fml");
        }
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_tdiv))
        rust_add_target_info("target_feature", "hwdiv");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_adiv))
        rust_add_target_info("target_feature", "hwdiv-arm");
    // TODO: I'm not sure if there's an exact correlation here (data barrier), so maybe research
    // There's also the question of whether this also means "full data barrier" ("fdb" in llvm)
    if (TARGET_HAVE_MEMORY_BARRIER)
        rust_add_target_info("target_feature", "db");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cmse))
        rust_add_target_info("target_feature", "8msecext");
    /* TODO: note that sha2 is an option for aarch64 in gcc but not for arm, so no feature here
     * possible. The same goes for aes. However, as llvm has them as prerequisites for crypto, they
     * are enabled with it. */
    if (TARGET_CRYPTO) {
        rust_add_target_info("target_feature", "crypto");
        rust_add_target_info("target_feature", "sha2");
        rust_add_target_info("target_feature", "aes");
    }
    if (TARGET_CRC32)
        rust_add_target_info("target_feature", "crc");
    if (TARGET_DOTPROD)
        rust_add_target_info("target_feature", "dotprod");
    // TODO: supposedly gcc supports RAS, but I couldn't find the option, so leaving out "ras" for now
    if (TARGET_DSP_MULTIPLY)
        rust_add_target_info("target_feature", "dsp");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_mp))
        rust_add_target_info("target_feature", "mp");
    // TODO: figure out the exact strict-align feature, which I'm pretty sure GCC has
    // TODO: figure out how to access long call data (which is in GCC) for "long-calls"
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_sb))
        rust_add_target_info("target_feature", "sb");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_bf16))
        rust_add_target_info("target_feature", "bf16");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_i8mm))
        rust_add_target_info("target_feature", "i8mm");
    switch (TARGET_ARM_ARCH_PROFILE) {
        case 'A':
            rust_add_target_info("target_feature", "aclass");
            break;
        case 'R':
            rust_add_target_info("target_feature", "rclass");
            break;
        case 'M':
            rust_add_target_info("target_feature", "mclass");
            break;
        default:
            fprintf(stderr, "Screwed up profile selection in arm-rust.c - unknown profile '%c'",
              TARGET_ARM_ARCH_PROFILE);
            break;
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_thumb2))
        rust_add_target_info("target_feature", "thumb2");
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv4)
        && bitmap_bit_p(arm_active_target.isa, isa_bit_notm)
        && bitmap_bit_p(arm_active_target.isa, isa_bit_thumb)) {
        rust_add_target_info("target_feature", "v4t");

        if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv5t)) {
            rust_add_target_info("target_feature", "v5t");

            if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv5te)) {
                rust_add_target_info("target_feature", "v5te");

                if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv6)
                    && bitmap_bit_p(arm_active_target.isa, isa_bit_be8)) {
                    rust_add_target_info("target_feature", "v6");

                    // note: this definition of "ARMv6m" listed as "suspect" in arm-cpus.in
                    rust_add_target_info("target_feature", "v6m");

                    bool hasV8BaselineOps = bitmap_bit_p(arm_active_target.isa, isa_bit_armv8)
                                            && bitmap_bit_p(arm_active_target.isa, isa_bit_cmse)
                                            && bitmap_bit_p(arm_active_target.isa, isa_bit_tdiv);
                    if (hasV8BaselineOps)
                        rust_add_target_info("target_feature", "v8m");

                    bool hasV6kOps = bitmap_bit_p(arm_active_target.isa, isa_bit_armv6k);
                    if (hasV6kOps) {
                        rust_add_target_info("target_feature", "v6k");
                    }

                    if (bitmap_bit_p(arm_active_target.isa, isa_bit_thumb2) && hasV8BaselineOps
                        && hasV6kOps) {
                        rust_add_target_info("target_feature", "v6t2");

                        // note that arm-cpus.in refers to this (ARMv7) as suspect
                        if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv7)) {
                            rust_add_target_info("target_feature", "v7");

                            rust_add_target_info("target_feature", "v8m.main");

                            if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv8_1m_main))
                                rust_add_target_info("target_feature", "v8.1m.main");

                            // dummy: can't find feature acquire-release, so dummy true variable
                            bool hasAcquireRelease = true;
                            if (hasAcquireRelease && bitmap_bit_p(arm_active_target.isa, isa_bit_adiv)
                                && bitmap_bit_p(arm_active_target.isa, isa_bit_lpae)
                                && bitmap_bit_p(arm_active_target.isa, isa_bit_mp)
                                && bitmap_bit_p(arm_active_target.isa, isa_bit_sec)) {
                                rust_add_target_info("target_feature", "v8");

                                if (TARGET_CRC32
                                    && bitmap_bit_p(arm_active_target.isa, isa_bit_armv8_1)) {
                                    rust_add_target_info("target_feature", "v8.1a");

                                    if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv8_2)) {
                                        rust_add_target_info("target_feature", "v8.2a");

                                        if (bitmap_bit_p(arm_active_target.isa, isa_bit_armv8_3)) {
                                            rust_add_target_info("target_feature", "v8.3a");

                                            if (bitmap_bit_p(
                                                  arm_active_target.isa, isa_bit_armv8_4)) {
                                                rust_add_target_info("target_feature", "v8.4a");
                                                // note: llvm, but not gcc, also wants dotprod for
                                                // v8.4

                                                if (bitmap_bit_p(arm_active_target.isa, isa_bit_sb)
                                                    && bitmap_bit_p(
                                                      arm_active_target.isa, isa_bit_predres)
                                                    && bitmap_bit_p(
                                                      arm_active_target.isa, isa_bit_armv8_5)) {
                                                    rust_add_target_info("target_feature", "v8.5a");

                                                    if (bitmap_bit_p(
                                                          arm_active_target.isa, isa_bit_armv8_6))
                                                        rust_add_target_info(
                                                          "target_feature", "v8.6a");
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_mve)
        && bitmap_bit_p(arm_active_target.isa, isa_bit_vfp_base)
        && bitmap_bit_p(arm_active_target.isa, isa_bit_armv7em)) {
        rust_add_target_info("target_feature", "mve");

        if (minFP_ARMv8 && bitmap_bit_p(arm_active_target.isa, isa_bit_fp16)
            && bitmap_bit_p(arm_active_target.isa, isa_bit_mve_float))
            rust_add_target_info("target_feature", "mve.fp");
    }
    // Note: no direct option for "cde" found, but it is implicitly activated via cdecpx, so do it
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp0)) {
        rust_add_target_info("target_feature", "cdecp0");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp1)) {
        rust_add_target_info("target_feature", "cdecp1");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp2)) {
        rust_add_target_info("target_feature", "cdecp2");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp3)) {
        rust_add_target_info("target_feature", "cdecp3");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp4)) {
        rust_add_target_info("target_feature", "cdecp4");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp5)) {
        rust_add_target_info("target_feature", "cdecp5");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp6)) {
        rust_add_target_info("target_feature", "cdecp6");
        rust_add_target_info("target_feature", "cde");
    }
    if (bitmap_bit_p(arm_active_target.isa, isa_bit_cdecp7)) {
        rust_add_target_info("target_feature", "cdecp7");
        rust_add_target_info("target_feature", "cde");
    }
    if (TARGET_SOFT_FLOAT)
        rust_add_target_info("target_feature", "soft-float");
    // should be correct option (i.e. thumb mode rather than just thumb-aware) as TARGET_ARM is
    // inverse
    if (TARGET_THUMB)
        rust_add_target_info("target_feature", "thumb-mode");
    // TODO: consider doing the processors as target features, but honestly they don't seem to fit
}
