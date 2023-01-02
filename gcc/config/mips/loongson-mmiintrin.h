/* Intrinsics for Loongson MultiMedia extension Instructions operations.

   Copyright (C) 2008-2023 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

#ifndef _GCC_LOONGSON_MMIINTRIN_H
#define _GCC_LOONGSON_MMIINTRIN_H

#if !defined(__mips_loongson_mmi)
# error You must select -mloongson-mmi or -march=loongson2e/2f/3a to use\
 loongson-mmiintrin.h
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Vectors of unsigned bytes, halfwords and words.  */
typedef uint8_t uint8x8_t __attribute__((vector_size (8)));
typedef uint16_t uint16x4_t __attribute__((vector_size (8)));
typedef uint32_t uint32x2_t __attribute__((vector_size (8)));

/* Vectors of signed bytes, halfwords and words.  */
typedef int8_t int8x8_t __attribute__((vector_size (8)));
typedef int16_t int16x4_t __attribute__((vector_size (8)));
typedef int32_t int32x2_t __attribute__((vector_size (8)));

/* SIMD intrinsics.
   Unless otherwise noted, calls to the functions below will expand into
   precisely one machine instruction, modulo any moves required to
   satisfy register allocation constraints.  */

/* Pack with signed saturation.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
packsswh (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_packsswh (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
packsshb (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_packsshb (s, t);
}

/* Pack with unsigned saturation.  */
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
packushb (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_packushb (s, t);
}

/* Vector addition, treating overflow by wraparound.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
paddw_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_paddw_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
paddh_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_paddh_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
paddb_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_paddb_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
paddw_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_paddw_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
paddh_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_paddh_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
paddb_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_paddb_s (s, t);
}

/* Addition of doubleword integers, treating overflow by wraparound.  */
__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
paddd_u (uint64_t s, uint64_t t)
{
  return __builtin_loongson_paddd_u (s, t);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
paddd_s (int64_t s, int64_t t)
{
  return __builtin_loongson_paddd_s (s, t);
}

/* Vector addition, treating overflow by signed saturation.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
paddsh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_paddsh (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
paddsb (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_paddsb (s, t);
}

/* Vector addition, treating overflow by unsigned saturation.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
paddush (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_paddush (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
paddusb (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_paddusb (s, t);
}

/* Logical AND NOT.  */
__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
pandn_ud (uint64_t s, uint64_t t)
{
  return __builtin_loongson_pandn_ud (s, t);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
pandn_uw (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_pandn_uw (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pandn_uh (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pandn_uh (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pandn_ub (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pandn_ub (s, t);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
pandn_sd (int64_t s, int64_t t)
{
  return __builtin_loongson_pandn_sd (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
pandn_sw (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_pandn_sw (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pandn_sh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pandn_sh (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
pandn_sb (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_pandn_sb (s, t);
}

/* Average.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pavgh (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pavgh (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pavgb (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pavgb (s, t);
}

/* Equality test.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
pcmpeqw_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_pcmpeqw_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pcmpeqh_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pcmpeqh_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pcmpeqb_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pcmpeqb_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
pcmpeqw_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_pcmpeqw_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pcmpeqh_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pcmpeqh_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
pcmpeqb_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_pcmpeqb_s (s, t);
}

/* Greater-than test.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
pcmpgtw_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_pcmpgtw_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pcmpgth_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pcmpgth_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pcmpgtb_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pcmpgtb_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
pcmpgtw_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_pcmpgtw_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pcmpgth_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pcmpgth_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
pcmpgtb_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_pcmpgtb_s (s, t);
}

/* Extract halfword.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pextrh_u (uint16x4_t s, int field /* 0--3.  */)
{
  return __builtin_loongson_pextrh_u (s, field);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pextrh_s (int16x4_t s, int field /* 0--3.  */)
{
  return __builtin_loongson_pextrh_s (s, field);
}

/* Insert halfword.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pinsrh_0_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pinsrh_0_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pinsrh_1_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pinsrh_1_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pinsrh_2_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pinsrh_2_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pinsrh_3_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pinsrh_3_u (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pinsrh_0_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pinsrh_0_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pinsrh_1_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pinsrh_1_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pinsrh_2_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pinsrh_2_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pinsrh_3_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pinsrh_3_s (s, t);
}

/* Multiply and add.  */
__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
pmaddhw (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pmaddhw (s, t);
}

/* Maximum of signed halfwords.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pmaxsh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pmaxsh (s, t);
}

/* Maximum of unsigned bytes.  */
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pmaxub (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pmaxub (s, t);
}

/* Minimum of signed halfwords.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pminsh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pminsh (s, t);
}

/* Minimum of unsigned bytes.  */
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pminub (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pminub (s, t);
}

/* Move byte mask.  */
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pmovmskb_u (uint8x8_t s)
{
  return __builtin_loongson_pmovmskb_u (s);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
pmovmskb_s (int8x8_t s)
{
  return __builtin_loongson_pmovmskb_s (s);
}

/* Multiply unsigned integers and store high result.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pmulhuh (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_pmulhuh (s, t);
}

/* Multiply signed integers and store high result.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pmulhh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pmulhh (s, t);
}

/* Multiply signed integers and store low result.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pmullh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_pmullh (s, t);
}

/* Multiply unsigned word integers.  */
__extension__ static __inline int64_t __attribute__ ((__always_inline__))
pmuluw (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_pmuluw (s, t);
}

/* Absolute difference.  */
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
pasubub (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_pasubub (s, t);
}

/* Sum of unsigned byte integers.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
biadd (uint8x8_t s)
{
  return __builtin_loongson_biadd (s);
}

/* Sum of absolute differences.
   Note that this intrinsic expands into two machine instructions:
   PASUBUB followed by BIADD.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psadbh (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_psadbh (s, t);
}

/* Shuffle halfwords.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
pshufh_u (uint16x4_t dest, uint16x4_t s, uint8_t order)
{
  return __builtin_loongson_pshufh_u (s, order);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
pshufh_s (int16x4_t dest, int16x4_t s, uint8_t order)
{
  return __builtin_loongson_pshufh_s (s, order);
}

/* Shift left logical.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psllh_u (uint16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psllh_u (s, amount);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
psllh_s (int16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psllh_s (s, amount);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
psllw_u (uint32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psllw_u (s, amount);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
psllw_s (int32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psllw_s (s, amount);
}

/* Shift right logical.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psrlh_u (uint16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psrlh_u (s, amount);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
psrlh_s (int16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psrlh_s (s, amount);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
psrlw_u (uint32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psrlw_u (s, amount);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
psrlw_s (int32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psrlw_s (s, amount);
}

/* Shift right arithmetic.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psrah_u (uint16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psrah_u (s, amount);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
psrah_s (int16x4_t s, uint8_t amount)
{
  return __builtin_loongson_psrah_s (s, amount);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
psraw_u (uint32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psraw_u (s, amount);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
psraw_s (int32x2_t s, uint8_t amount)
{
  return __builtin_loongson_psraw_s (s, amount);
}

/* Vector subtraction, treating overflow by wraparound.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
psubw_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_psubw_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psubh_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_psubh_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
psubb_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_psubb_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
psubw_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_psubw_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
psubh_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_psubh_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
psubb_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_psubb_s (s, t);
}

/* Subtraction of doubleword integers, treating overflow by wraparound.  */
__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
psubd_u (uint64_t s, uint64_t t)
{
  return __builtin_loongson_psubd_u (s, t);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
psubd_s (int64_t s, int64_t t)
{
  return __builtin_loongson_psubd_s (s, t);
}

/* Vector subtraction, treating overflow by signed saturation.  */
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
psubsh (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_psubsh (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
psubsb (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_psubsb (s, t);
}

/* Vector subtraction, treating overflow by unsigned saturation.  */
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
psubush (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_psubush (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
psubusb (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_psubusb (s, t);
}

/* Unpack high data.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
punpckhwd_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_punpckhwd_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
punpckhhw_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_punpckhhw_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
punpckhbh_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_punpckhbh_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
punpckhwd_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_punpckhwd_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
punpckhhw_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_punpckhhw_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
punpckhbh_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_punpckhbh_s (s, t);
}

/* Unpack low data.  */
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
punpcklwd_u (uint32x2_t s, uint32x2_t t)
{
  return __builtin_loongson_punpcklwd_u (s, t);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
punpcklhw_u (uint16x4_t s, uint16x4_t t)
{
  return __builtin_loongson_punpcklhw_u (s, t);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
punpcklbh_u (uint8x8_t s, uint8x8_t t)
{
  return __builtin_loongson_punpcklbh_u (s, t);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
punpcklwd_s (int32x2_t s, int32x2_t t)
{
  return __builtin_loongson_punpcklwd_s (s, t);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
punpcklhw_s (int16x4_t s, int16x4_t t)
{
  return __builtin_loongson_punpcklhw_s (s, t);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
punpcklbh_s (int8x8_t s, int8x8_t t)
{
  return __builtin_loongson_punpcklbh_s (s, t);
}

#ifdef __cplusplus
}
#endif

#endif
