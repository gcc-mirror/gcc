/* ISA feature bits for ARM.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef ARM_ISA_FEATURE_H
#define ARM_ISA_FEATURE_H

enum isa_feature
  {
    isa_nobit,		/* Must be first.  */
    isa_bit_ARMv3m,	/* Extended multiply.  */
    isa_bit_mode26,	/* 26-bit mode support.  */
    isa_bit_mode32,	/* 32-bit mode support.  */
    isa_bit_ARMv4,	/* Architecture rel 4.  */
    isa_bit_ARMv5,	/* Architecture rel 5.	*/
    isa_bit_thumb,	/* Thumb aware.  */
    isa_bit_ARMv5e,	/* Architecture rel 5e.  */
    isa_bit_xscale,	/* XScale.  */
    isa_bit_ARMv6,	/* Architecture rel 6.  */
    isa_bit_ARMv6k,	/* Architecture rel 6k.  */
    isa_bit_thumb2,	/* Thumb-2.  */
    isa_bit_notm,	/* Instructions that are not present in 'M' profile.  */
    isa_bit_tdiv,	/* Thumb division instructions.  */
    isa_bit_ARMv7em,	/* Architecture rel 7e-m.  */
    isa_bit_ARMv7,	/* Architecture rel 7.  */
    isa_bit_adiv,	/* ARM division instructions.  */
    isa_bit_ARMv8,	/* Architecture rel 8.  */
    isa_bit_crc32,	/* ARMv8 CRC32 instructions.  */
    isa_bit_iwmmxt,	/* XScale v2 (Wireless MMX).  */
    isa_bit_iwmmxt2,	/* XScale Wireless MMX2.  */
    isa_bit_ARMv8_1,	/* Architecture rel 8.1.  */
    isa_bit_ARMv8_2,	/* Architecutre rel 8.2.  */
    isa_bit_cmse,	/* M-Profile security extensions.  */
    /* Floating point and Neon extensions.  */
    /* VFPv1 is not supported in GCC.  */
    isa_bit_VFPv2,	/* Vector floating point v2.  */
    isa_bit_VFPv3,	/* Vector floating point v3.  */
    isa_bit_VFPv4,	/* Vector floating point v4.  */
    isa_bit_FPv5,	/* Floating point v5.  */
    isa_bit_lpae,	/* ARMv7-A LPAE.  */
    isa_bit_FP_ARMv8,	/* ARMv8 floating-point extension.  */
    isa_bit_neon,	/* Advanced SIMD instructions.  */
    isa_bit_fp16conv,	/* Conversions to/from fp16 (VFPv3 extension).  */
    isa_bit_fp_dbl,	/* Double precision operations supported.  */
    isa_bit_fp_d32,	/* 32 Double precision registers.  */
    isa_bit_crypto,	/* Crypto extension to ARMv8.  */
    isa_bit_fp16,	/* FP16 data processing (half-precision float).  */

    /* ISA Quirks (errata?).  Don't forget to add this to the list of
       all quirks below.  */
    isa_quirk_no_volatile_ce,	/* No volatile memory in IT blocks.  */
    isa_quirk_ARMv6kz,		/* Previously mis-identified by GCC.  */
    isa_quirk_cm3_ldrd,		/* Cortex-M3 LDRD quirk.  */

    /* Aren't currently, but probably should be tuning bits.  */
    isa_bit_smallmul,	/* Slow multiply operations.  */

    /* Tuning bits.  Should be elsewhere.  */
    isa_tune_co_proc,	/* Has co-processor bus.  */
    isa_tune_ldsched,	/* Load scheduling necessary.  */
    isa_tune_strong,	/* StrongARM.  */
    isa_tune_wbuf,	/* Schedule for write buffer ops (ARM6 & 7 only).  */

    /* Must be last, used to dimension arrays.  */
    isa_num_bits
  };

/* Helper macros for use when defining CPUs and architectures.

   There must be no parenthesees in these lists, since they are used
   to initialize arrays.  */

#define ISA_ARMv2	isa_bit_notm
#define ISA_ARMv3	ISA_ARMv2, isa_bit_mode32
#define ISA_ARMv3m	ISA_ARMv3, isa_bit_ARMv3m
#define ISA_ARMv4	ISA_ARMv3m, isa_bit_ARMv4
#define ISA_ARMv4t	ISA_ARMv4, isa_bit_thumb
#define ISA_ARMv5	ISA_ARMv4, isa_bit_ARMv5
#define ISA_ARMv5t	ISA_ARMv5, isa_bit_thumb
#define ISA_ARMv5e	ISA_ARMv5, isa_bit_ARMv5e
#define ISA_ARMv5te	ISA_ARMv5e, isa_bit_thumb
#define ISA_ARMv5tej	ISA_ARMv5te
#define ISA_ARMv6	ISA_ARMv5te, isa_bit_ARMv6
#define ISA_ARMv6j	ISA_ARMv6
#define ISA_ARMv6k	ISA_ARMv6, isa_bit_ARMv6k
#define ISA_ARMv6z	ISA_ARMv6
#define ISA_ARMv6kz	ISA_ARMv6k, isa_quirk_ARMv6kz
#define ISA_ARMv6zk	ISA_ARMv6k
#define ISA_ARMv6t2	ISA_ARMv6, isa_bit_thumb2

/* This is suspect.  ARMv6-m doesn't really pull in any useful features
   from ARMv5* or ARMv6.  */
#define ISA_ARMv6m	isa_bit_mode32, isa_bit_ARMv3m, isa_bit_ARMv4, \
    isa_bit_thumb, isa_bit_ARMv5, isa_bit_ARMv5e, isa_bit_ARMv6
/* This is suspect, the 'common' ARMv7 subset excludes the thumb2 'DSP' and
   integer SIMD instructions that are in ARMv6T2.  */
#define ISA_ARMv7	ISA_ARMv6m, isa_bit_thumb2, isa_bit_ARMv7
#define ISA_ARMv7a	ISA_ARMv7, isa_bit_notm, isa_bit_ARMv6k
#define ISA_ARMv7ve	ISA_ARMv7a, isa_bit_adiv, isa_bit_tdiv, isa_bit_lpae
#define ISA_ARMv7r	ISA_ARMv7a, isa_bit_tdiv
#define ISA_ARMv7m	ISA_ARMv7, isa_bit_tdiv
#define ISA_ARMv7em	ISA_ARMv7m, isa_bit_ARMv7em
#define ISA_ARMv8a	ISA_ARMv7ve, isa_bit_ARMv8
#define ISA_ARMv8_1a	ISA_ARMv8a, isa_bit_crc32, isa_bit_ARMv8_1
#define ISA_ARMv8_2a	ISA_ARMv8_1a, isa_bit_ARMv8_2
#define ISA_ARMv8m_base ISA_ARMv6m, isa_bit_ARMv8, isa_bit_cmse, isa_bit_tdiv
#define ISA_ARMv8m_main ISA_ARMv7m, isa_bit_ARMv8, isa_bit_cmse

/* List of all FPU bits to strip out if -mfpu is used to override the
   default.  isa_bit_fp16 is deliberately missing from this list.  */
#define ISA_ALL_FPU	isa_bit_VFPv2, isa_bit_VFPv3, isa_bit_VFPv4, \
    isa_bit_FPv5, isa_bit_FP_ARMv8, isa_bit_neon, isa_bit_fp16conv, \
    isa_bit_fp_dbl, isa_bit_fp_d32, isa_bit_crypto

/* Useful combinations.  */
#define ISA_VFPv2	isa_bit_VFPv2
#define ISA_VFPv3	ISA_VFPv2, isa_bit_VFPv3
#define ISA_VFPv4	ISA_VFPv3, isa_bit_VFPv4, isa_bit_fp16conv
#define ISA_FPv5	ISA_VFPv4, isa_bit_FPv5
#define ISA_FP_ARMv8	ISA_FPv5, isa_bit_FP_ARMv8

#define ISA_FP_DBL	isa_bit_fp_dbl
#define ISA_FP_D32	ISA_FP_DBL, isa_bit_fp_d32
#define ISA_NEON	ISA_FP_D32, isa_bit_neon
#define ISA_CRYPTO	ISA_NEON, isa_bit_crypto

/* List of all quirk bits to strip out when comparing CPU features with
   architectures.  */
#define ISA_ALL_QUIRKS	isa_quirk_no_volatile_ce, isa_quirk_ARMv6kz,	\
    isa_quirk_cm3_ldrd

/* Helper macro so that we can concatenate multiple features together
   with arm-*.def files, since macro substitution can't have commas within an
   argument that lacks parenthesis.  */
#define ISA_FEAT(X)	X,
#endif
