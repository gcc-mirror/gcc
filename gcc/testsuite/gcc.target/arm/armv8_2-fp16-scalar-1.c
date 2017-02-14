/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

/* Test instructions generated for the FP16 scalar intrinsics.  */
#include <arm_fp16.h>

#define MSTRCAT(L, str)	L##str

#define UNOP_TEST(insn)				\
  float16_t					\
  MSTRCAT (test_##insn, 16) (float16_t a)	\
  {						\
    return MSTRCAT (insn, h_f16) (a);		\
  }

#define BINOP_TEST(insn)				\
  float16_t						\
  MSTRCAT (test_##insn, 16) (float16_t a, float16_t b)	\
  {							\
    return MSTRCAT (insn, h_f16) (a, b);		\
  }

#define TERNOP_TEST(insn)						\
  float16_t								\
  MSTRCAT (test_##insn, 16) (float16_t a, float16_t b, float16_t c)	\
  {									\
    return MSTRCAT (insn, h_f16) (a, b, c);				\
  }

float16_t
test_vcvth_f16_s32 (int32_t a)
{
  return vcvth_f16_s32 (a);
}

float16_t
test_vcvth_n_f16_s32 (int32_t a)
{
  return vcvth_n_f16_s32 (a, 1);
}
/* { dg-final { scan-assembler-times {vcvt\.f16\.s32\ts[0-9]+, s[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcvt\.f16\.s32\ts[0-9]+, s[0-9]+, #1} 1 } }  */

float16_t
test_vcvth_f16_u32 (uint32_t a)
{
  return vcvth_f16_u32 (a);
}

float16_t
test_vcvth_n_f16_u32 (uint32_t a)
{
  return vcvth_n_f16_u32 (a, 1);
}

/* { dg-final { scan-assembler-times {vcvt\.f16\.u32\ts[0-9]+, s[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcvt\.f16\.u32\ts[0-9]+, s[0-9]+, #1} 1 } }  */

uint32_t
test_vcvth_u32_f16 (float16_t a)
{
  return vcvth_u32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvt\.u32\.f16\ts[0-9]+, s[0-9]+} 2 } }  */

uint32_t
test_vcvth_n_u32_f16 (float16_t a)
{
  return vcvth_n_u32_f16 (a, 1);
}
/* { dg-final { scan-assembler-times {vcvt\.u32\.f16\ts[0-9]+, s[0-9]+, #1} 1 } }  */

int32_t
test_vcvth_s32_f16 (float16_t a)
{
  return vcvth_s32_f16 (a);
}

int32_t
test_vcvth_n_s32_f16 (float16_t a)
{
  return vcvth_n_s32_f16 (a, 1);
}

/* { dg-final { scan-assembler-times {vcvt\.s32\.f16\ts[0-9]+, s[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcvt\.s32\.f16\ts[0-9]+, s[0-9]+, #1} 1 } }  */

int32_t
test_vcvtah_s32_f16 (float16_t a)
{
  return vcvtah_s32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvta\.s32\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

uint32_t
test_vcvtah_u32_f16 (float16_t a)
{
  return vcvtah_u32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvta\.u32\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

int32_t
test_vcvtmh_s32_f16 (float16_t a)
{
  return vcvtmh_s32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtm\.s32\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

uint32_t
test_vcvtmh_u32_f16 (float16_t a)
{
  return vcvtmh_u32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtm\.u32\.f16\ts[0-9]+, s[0-9]+} 1 } }
 */

int32_t
test_vcvtnh_s32_f16 (float16_t a)
{
  return vcvtnh_s32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtn\.s32\.f16\ts[0-9]+, s[0-9]+} 1 } }
 */

uint32_t
test_vcvtnh_u32_f16 (float16_t a)
{
  return vcvtnh_u32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtn\.u32\.f16\ts[0-9]+, s[0-9]+} 1 } }
 */

int32_t
test_vcvtph_s32_f16 (float16_t a)
{
  return vcvtph_s32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtp\.s32\.f16\ts[0-9]+, s[0-9]+} 1 } }
 */

uint32_t
test_vcvtph_u32_f16 (float16_t a)
{
  return vcvtph_u32_f16 (a);
}
/* { dg-final { scan-assembler-times {vcvtp\.u32\.f16\ts[0-9]+, s[0-9]+} 1 } }
 */

UNOP_TEST (vabs)
/* { dg-final { scan-assembler-times {vabs\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vneg)
/* { dg-final { scan-assembler-times {vneg\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrnd)
/* { dg-final { scan-assembler-times {vrintz\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrndi)
/* { dg-final { scan-assembler-times {vrintr\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrnda)
/* { dg-final { scan-assembler-times {vrinta\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrndm)
/* { dg-final { scan-assembler-times {vrinta\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrndn)
/* { dg-final { scan-assembler-times {vrinta\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrndp)
/* { dg-final { scan-assembler-times {vrinta\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vrndx)
/* { dg-final { scan-assembler-times {vrinta\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

UNOP_TEST (vsqrt)
/* { dg-final { scan-assembler-times {vsqrt\.f16\ts[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vadd)
/* { dg-final { scan-assembler-times {vadd\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vdiv)
/* { dg-final { scan-assembler-times {vdiv\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vmaxnm)
/* { dg-final { scan-assembler-times {vmaxnm\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vminnm)
/* { dg-final { scan-assembler-times {vminnm\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vmul)
/* { dg-final { scan-assembler-times {vmul\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

BINOP_TEST (vsub)
/* { dg-final { scan-assembler-times {vsub\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

TERNOP_TEST (vfma)
/* { dg-final { scan-assembler-times {vfma\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */

TERNOP_TEST (vfms)
/* { dg-final { scan-assembler-times {vfms\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
