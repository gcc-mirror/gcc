#include "arm_cde.h"

/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-mfpu=auto" } */

/* Test that the assembly is produced as expected.
   Test that the same thing happens for each valid type.
     (ensure we check *every* valid type, though we're not bothering with every
     type combination, just checking "all same type" and "different types",
     also want to check every valid type at least once)  */

/* Use every valid type for the output -- demonstrate can use any 128 bit value
   (which is a requirement for these intrinsics).  */
#define TEST_CDE_MVE_INTRINSIC_1(name, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, __builtin_neon_ti, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float16x8_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float32x4_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint16x8_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint32x4_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint64x2_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int8x16_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int16x8_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int32x4_t, int, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int64x2_t, int, int, arguments) \

#define TEST_CDE_MVE_INTRINSIC_2(name, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, __builtin_neon_ti, uint8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, __builtin_neon_ti, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float16x8_t, uint16x8_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float16x8_t, float32x4_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float32x4_t, uint8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int64x2_t, uint8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int8x16_t, uint8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint16x8_t, uint8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int64x2_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int8x16_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint16x8_t, int, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, int, arguments)

#define TEST_CDE_MVE_INTRINSIC_3(name, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, __builtin_neon_ti, uint8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, __builtin_neon_ti, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float16x8_t, float16x8_t, float16x8_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, float32x4_t, uint64x2_t, float16x8_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint16x8_t, uint8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint16x8_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, uint16x8_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int8x16_t, uint8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, int8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, int64x2_t, uint8x16_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int64x2_t, uint8x16_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, uint8x16_t, int64x2_t, arguments) \
  TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, uint8x16_t, int64x2_t, int64x2_t, arguments)


#define TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, accum_type, n_type, m_type, arguments) \
  accum_type test_cde_##name##accum_type##n_type##m_type ( \
		      __attribute__ ((unused)) n_type n, \
		      __attribute__ ((unused)) m_type m) \
  {   \
    accum_type accum = (accum_type)(uint32x4_t){0,0,0,0};  \
    accum += (accum_type) __arm_##name arguments; \
    return accum; \
  }

TEST_CDE_MVE_INTRINSIC_1(vcx1q_u8, (0, 33))
TEST_CDE_MVE_INTRINSIC_1(vcx1qa, (0, accum, 33))

TEST_CDE_MVE_INTRINSIC_2(vcx2q_u8, (0, n, 33))
TEST_CDE_MVE_INTRINSIC_2(vcx2q, (0, n, 33))
TEST_CDE_MVE_INTRINSIC_2(vcx2qa, (0, accum, n, 33))

TEST_CDE_MVE_INTRINSIC_3(vcx3q_u8, (0, n, m, 12))
TEST_CDE_MVE_INTRINSIC_3(vcx3q, (0, n, m, 12))
TEST_CDE_MVE_INTRINSIC_3(vcx3qa, (0, accum, n, m, 12))

#undef TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE
#define TEST_CDE_MVE_INTRINSIC_SPECIFIED_TYPE(name, accum_type, n_type, m_type, arguments) \
  accum_type test_cde_##name##accum_type##n_type##m_type ( \
		      __attribute__ ((unused)) n_type n, \
		      __attribute__ ((unused)) m_type m, \
		      mve_pred16_t pred) \
  {   \
    accum_type accum = (accum_type)(uint32x4_t){0,0,0,0};  \
    accum += (accum_type) __arm_##name arguments; \
    return accum; \
  }

TEST_CDE_MVE_INTRINSIC_1(vcx1q_m, (0, accum, 32, pred))
TEST_CDE_MVE_INTRINSIC_1(vcx1qa_m, (0, accum, 32, pred))

TEST_CDE_MVE_INTRINSIC_2(vcx2q_m, (0, accum, n, 32, pred))
TEST_CDE_MVE_INTRINSIC_2(vcx2qa_m, (0, accum, n, 32, pred))

TEST_CDE_MVE_INTRINSIC_3(vcx3q_m, (0, accum, n, m, 15, pred))
TEST_CDE_MVE_INTRINSIC_3(vcx3qa_m, (0, accum, n, m, 15, pred))

/* This testcase checks that in all compilations this C code produces the
   expected CDE instructions from the above intrinsics.

   Here we check that there are the expected number of `vcx*` occurences, and
   that each function has the expected form in it.

   Another testcase (cde-mve-full-assembly.c) checks that when using
   `-mfloat-abi=hard` and when compiled with an FPU the above C code produces
   code that demonstrates the compiler knows that the intrinsics are constant
   and pure, and that demonstrates the compiler generates sane code from them.
   That testcase needs these special arguments so it can ignore things like
   accounting for the soft float ABI or leftovers from temporaries that are
   later removed when generating code for a target with Floating Point
   registers but without an FPU.  */

/* { dg-final { scan-assembler-times "\tvcx1\t" 11 } } */
/* { dg-final { scan-assembler-times "\tvcx1a\t" 11 } } */
/* { dg-final { scan-assembler-times "\tvcx2\t" 24 } } */
/* { dg-final { scan-assembler-times "\tvcx2a\t" 12 } } */
/* { dg-final { scan-assembler-times "\tvcx3\t" 30 } } */
/* { dg-final { scan-assembler-times "\tvcx3a\t" 15 } } */

/* { dg-final { scan-assembler-times "\tvcx1t\t" 11 } } */
/* { dg-final { scan-assembler-times "\tvcx1at\t" 11 } } */
/* { dg-final { scan-assembler-times "\tvcx2t\t" 12 } } */
/* { dg-final { scan-assembler-times "\tvcx2at\t" 12 } } */
/* { dg-final { scan-assembler-times "\tvcx3t\t" 15 } } */
/* { dg-final { scan-assembler-times "\tvcx3at\t" 15 } } */

/*
** test_cde_vcx1q_u8__builtin_neon_tiintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8float16x8_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8float32x4_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8uint8x16_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8uint16x8_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8uint32x4_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8uint64x2_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8int8x16_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8int16x8_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8int32x4_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1q_u8int64x2_tintint:
** 	...
** 	vcx1	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qa__builtin_neon_tiintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qafloat16x8_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qafloat32x4_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qauint8x16_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qauint16x8_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qauint32x4_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qauint64x2_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qaint8x16_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qaint16x8_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qaint32x4_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx1qaint64x2_tintint:
** 	...
** 	vcx1a	p0, q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8__builtin_neon_tiuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint8x16_t__builtin_neon_tiint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8float16x8_tuint16x8_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8float16x8_tfloat32x4_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8float32x4_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8int64x2_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8int8x16_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint16x8_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint8x16_tint64x2_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint8x16_tint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint8x16_tuint16x8_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q_u8uint8x16_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2q__builtin_neon_tiuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint8x16_t__builtin_neon_tiint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qfloat16x8_tuint16x8_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qfloat16x8_tfloat32x4_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qfloat32x4_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qint64x2_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qint8x16_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint16x8_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint8x16_tint64x2_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint8x16_tint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint8x16_tuint16x8_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2quint8x16_tuint8x16_tint:
** 	...
** 	vcx2	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qa__builtin_neon_tiuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint8x16_t__builtin_neon_tiint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qafloat16x8_tuint16x8_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qafloat16x8_tfloat32x4_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qafloat32x4_tuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qaint64x2_tuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qaint8x16_tuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint16x8_tuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint8x16_tint64x2_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint8x16_tint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint8x16_tuint16x8_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx2qauint8x16_tuint8x16_tint:
** 	...
** 	vcx2a	p0, q[0-7], q[0-7], #33
** 	...
*/
/*
** test_cde_vcx3q_u8__builtin_neon_tiuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_t__builtin_neon_ti:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint16x8_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint16x8_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tuint16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8float16x8_tfloat16x8_tfloat16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8float32x4_tuint64x2_tfloat16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8int8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8int64x2_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tint64x2_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tuint8x16_tint64x2_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q_u8uint8x16_tint64x2_tint64x2_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3q__builtin_neon_tiuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_t__builtin_neon_ti:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qfloat16x8_tfloat16x8_tfloat16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qfloat32x4_tuint64x2_tfloat16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint16x8_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint16x8_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tuint16x8_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qint64x2_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tint64x2_tuint8x16_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tuint8x16_tint64x2_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3quint8x16_tint64x2_tint64x2_t:
** 	...
** 	vcx3	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qa__builtin_neon_tiuint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_t__builtin_neon_ti:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qafloat16x8_tfloat16x8_tfloat16x8_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qafloat32x4_tuint64x2_tfloat16x8_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint16x8_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint16x8_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tuint16x8_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qaint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qaint64x2_tuint8x16_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tint64x2_tuint8x16_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tuint8x16_tint64x2_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/
/*
** test_cde_vcx3qauint8x16_tint64x2_tint64x2_t:
** 	...
** 	vcx3a	p0, q[0-7], q[0-7], q[0-7], #12
** 	...
*/

/*
** test_cde_vcx1q_m__builtin_neon_tiintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mfloat16x8_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mfloat32x4_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_muint8x16_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_muint16x8_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_muint32x4_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_muint64x2_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mint8x16_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mint16x8_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mint32x4_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1q_mint64x2_tintint:
** 	...
** 	vpst
** 	vcx1t	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_m__builtin_neon_tiintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mfloat16x8_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mfloat32x4_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_muint8x16_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_muint16x8_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_muint32x4_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_muint64x2_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mint8x16_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mint16x8_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mint32x4_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx1qa_mint64x2_tintint:
** 	...
** 	vpst
** 	vcx1at	p0, q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_m__builtin_neon_tiuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint8x16_t__builtin_neon_tiint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_mfloat16x8_tuint16x8_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_mfloat16x8_tfloat32x4_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_mfloat32x4_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_mint64x2_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_mint8x16_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint16x8_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint8x16_tint64x2_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint8x16_tint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint8x16_tuint16x8_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2q_muint8x16_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2t	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_m__builtin_neon_tiuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint8x16_t__builtin_neon_tiint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_mfloat16x8_tuint16x8_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_mfloat16x8_tfloat32x4_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_mfloat32x4_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_mint64x2_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_mint8x16_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint16x8_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint8x16_tint64x2_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint8x16_tint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint8x16_tuint16x8_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx2qa_muint8x16_tuint8x16_tint:
** 	...
** 	vpst
** 	vcx2at	p0, q[0-7], q[0-7], #32
** 	...
*/
/*
** test_cde_vcx3q_m__builtin_neon_tiuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_t__builtin_neon_ti:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_mfloat16x8_tfloat16x8_tfloat16x8_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_mfloat32x4_tuint64x2_tfloat16x8_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint16x8_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint16x8_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tuint16x8_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_mint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_mint64x2_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tint64x2_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tuint8x16_tint64x2_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3q_muint8x16_tint64x2_tint64x2_t:
** 	...
** 	vpst
** 	vcx3t	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_m__builtin_neon_tiuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_t__builtin_neon_ti:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_mfloat16x8_tfloat16x8_tfloat16x8_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_mfloat32x4_tuint64x2_tfloat16x8_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint16x8_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint16x8_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tuint16x8_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_mint8x16_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_mint64x2_tuint8x16_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tint64x2_tuint8x16_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tuint8x16_tint64x2_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
/*
** test_cde_vcx3qa_muint8x16_tint64x2_tint64x2_t:
** 	...
** 	vpst
** 	vcx3at	p0, q[0-7], q[0-7], q[0-7], #15
** 	...
*/
