/* Subroutines for the Rust front end for the MIPS architecture.
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

/* Add target info for MIPS-I ISA support.  */

static void mips1_target_info(void) {
    rust_add_target_info("target_feature", "mips1");
}

/* Add target info for MIPS-II ISA support.  */

static void mips2_target_info(void) {
    rust_add_target_info("target_feature", "mips2");
    mips1_target_info();
}

/* Add target info for MIPS-III ISA (MIPS32 subset) support.  */

static void mips3_32_target_info(void) {
    rust_add_target_info("target_feature", "mips3_32");
}

/* Add target info for MIPS-III ISA (MIPS32r2 subset) support.  */

static void mips3_32r2_target_info(void) {
    rust_add_target_info("target_feature", "mips3_32r2");
}

/* Add target info for MIPS-III ISA support.  */

static void mips3_target_info(void) {
    rust_add_target_info("target_feature", "mips3");
    mips2_target_info();
    mips3_32_target_info();
    mips3_32r2_target_info();
    // maybe always have FeatureGP64Bit and FeatureFP64Bit? or maybe make them prereqs?
}

/* Add target info for MIPS-IV ISA (MIPS32 subset) support.  */

static void mips4_32_target_info(void) {
    rust_add_target_info("target_feature", "mips4_32");
}

/* Add target info for MIPS-IV ISA (MIPS32r2 subset) support.  */

static void mips4_32r2_target_info(void) {
    rust_add_target_info("target_feature", "mips4_32r2");
}

/* Add target info for MIPS-IV ISA support.  */

static void mips4_target_info(void) {
    rust_add_target_info("target_feature", "mips4");
    mips3_target_info();
    mips4_32_target_info();
    mips4_32r2_target_info();
}

/* Add target info for MIPS-V ISA (MIPS32r2 subset) support.  */

static void mips5_32r2_target_info(void) {
    rust_add_target_info("target_feature", "mips5_32r2");
}

/* Add target info for MIPS-V ISA support.  */

static void mips5_target_info(void) {
    rust_add_target_info("target_feature", "mips5");
    mips4_target_info();
    mips5_32r2_target_info();
}

/* Add target info for MIPS32 ISA support.  */

static void mips32_target_info(void) {
    rust_add_target_info("target_feature", "mips32");
    mips2_target_info();
    mips3_32_target_info();
    mips4_32_target_info();
}

/* Add target info for MIPS32r2 ISA support.  */

static void mips32r2_target_info(void) {
    rust_add_target_info("target_feature", "mips32r2");
    mips32_target_info();
    mips3_32r2_target_info();
    mips4_32r2_target_info();
    mips5_32r2_target_info();
}

/* Add target info for MIPS32r3 ISA support.  */

static void mips32r3_target_info(void) {
    rust_add_target_info("target_feature", "mips32r3");
    mips32r2_target_info();
}

/* Add target info for MIPS32r5 ISA support.  */

static void mips32r5_target_info(void) {
    rust_add_target_info("target_feature", "mips32r5");
    mips32r3_target_info();
}

/* Add target info for MIPS32r6 ISA support.  */

static void mips32r6_target_info(void) {
    rust_add_target_info("target_feature", "mips32r6");
    mips32r5_target_info();
    // maybe also hardcode having FeatureFP64Bit, FeatureNaN2008, and FeatureAbs2008, or have prereqs
}

/* Add target info for MIPS64 ISA support.  */

static void mips64_target_info(void) {
    rust_add_target_info("target_feature", "mips64");
    mips32_target_info();
    mips5_target_info();
}

/* Add target info for MIPS64r2 ISA support.  */

static void mips64r2_target_info(void) {
    rust_add_target_info("target_feature", "mips64r2");
    mips64_target_info();
    mips32r2_target_info();
}

/* Add target info for MIPS64r3 ISA support.  */

static void mips64r3_target_info(void) {
    rust_add_target_info("target_feature", "mips64r3");
    mips64r2_target_info();
    mips32r3_target_info();
}

/* Add target info for MIPS64r5 ISA support.  */

static void mips64r5_target_info(void) {
    rust_add_target_info("target_feature", "mips64r5");
    mips64r3_target_info();
    mips32r5_target_info();
}

/* Add target info for MIPS64r6 ISA support.  */

static void mips64r6_target_info(void) {
    rust_add_target_info("target_feature", "mips64r6");
    mips64r5_target_info();
    mips32r6_target_info();
    // maybe hardcode FeatureNaN2008 and FeatureAbs2008, or just have them as prereqs?
}

/* Implement TARGET_RUST_CPU_INFO for MIPS targets.  */

void mips_rust_target_cpu_info(void) {
    if (TARGET_64BIT)
        rust_add_target_info("target_arch", "mips64");
    else
        rust_add_target_info("target_arch", "mips");

    // features and names based on llvm if not in rustc
    // TODO maybe implement more features that aren't in llvm but are in gcc at some point?
    if (!(TARGET_ABICALLS))
        rust_add_target_info("target_feature", "noabicalls");
    if (POINTER_SIZE == 64)
        rust_add_target_info("target_feature", "ptr64");
    if (TARGET_64BIT) 
        rust_add_target_info("target_feature", "gp64");
    if (TARGET_FLOAT64)
        rust_add_target_info("target_feature", "fp64");
    if (TARGET_FLOATXX)
        rust_add_target_info("target_feature", "fpxx");
    // TODO: ensure below variables work
    if (mips_nan == MIPS_IEEE_754_2008)
        rust_add_target_info("target_feature", "nan2008");
    if (mips_abs == MIPS_IEEE_754_2008)
        rust_add_target_info("target_feature", "abs2008");
    if (TARGET_SINGLE_FLOAT)
        rust_add_target_info("target_feature", "single-float");
    if (TARGET_SOFT_FLOAT_ABI)
        rust_add_target_info("target_feature", "soft-float");
    if (!TARGET_ODD_SPREG)
        rust_add_target_info("target_feature", "nooddspreg");
    // TODO: find if vfpu (vector FPU instructions) are supported by gcc at all
    // TODO: ensure below switch variable and whatever works
    switch (mips_isa_option) {
        case 0: 
            mips1_target_info();
            break;
        case 1: 
            mips2_target_info();
            break;
        case 2: 
            mips3_target_info();
            break;
        case 3: 
            mips4_target_info();
            break;
        case 4: 
            mips32_target_info();
            break;
        case 5: 
            mips32r2_target_info();
            break;
        case 6: 
            mips32r3_target_info();
            break;
        case 7: 
            mips32r5_target_info();
            break;
        case 8: 
            mips32r6_target_info();
            break;
        case 9: 
            mips64_target_info();
            break;
        case 10: 
            mips64r2_target_info();
            break;
        case 11: 
            mips64r3_target_info();
            break;
        case 12: 
            mips64r5_target_info();
            break;
        case 13: 
            mips64r6_target_info();
            break;
        default: // unknown isa level - should this be an error?
            break;
    }
    if (TARGET_SYM32)
        rust_add_target_info("target_feature", "sym32");
    if (TARGET_MIPS16)
        rust_add_target_info("target_feature", "mips16");
    if (TARGET_DSP) {
        rust_add_target_info("target_feature", "dsp");

        if (TARGET_DSPR2) {
            rust_add_target_info("target_feature", "dspr2");

            // TODO: add dspr3 if can find gcc equivalent option 
        }
    }
    if (TARGET_MIPS3D)
        rust_add_target_info("target_feature", "mips3d");
    if (TARGET_MSA)
        rust_add_target_info("target_feature", "msa");
    if (TARGET_EVA)
        rust_add_target_info("target_feature", "eva");
    if (TARGET_CRC)
        rust_add_target_info("target_feature", "crc");
    if (TARGET_VIRT)
        rust_add_target_info("target_feature", "virt");
    if (TARGET_GINV)
        rust_add_target_info("target_feature", "ginv");
    if (TARGET_MICROMIPS)
        rust_add_target_info("target_feature", "micromips");
    // TODO: add cnmips and cnmipsp if there are gcc equivalent features
    // TODO: find out what use-tcc-in-div ("force the assembler to use trapping") corresponds to
    // TODO ensure below variable works
    if (!mips_madd4)
        rust_add_target_info("target_feature", "nomadd4");   
    if (TARGET_MT)
        rust_add_target_info("target_feature", "mt");
    if (TARGET_LONG_CALLS)
        rust_add_target_info("target_feature", "long-calls");   
    if (TARGET_XGOT)
        rust_add_target_info("target_feature", "xgot");
    /* TODO: find out what use-indirect-jump-hazard corresponds with ("use indirect jump guards to prevent
     * certain speculation based attacks") */
}
