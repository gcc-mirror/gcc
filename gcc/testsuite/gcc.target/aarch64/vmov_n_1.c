/* Test vmov_n works correctly.  */
/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);

#define INHIB_OPTIMIZATION asm volatile ("" : : : "memory")

#define CONCAT(a, b) a##b
#define CONCAT1(a, b) CONCAT (a, b)
#define REG_INFEX64 _
#define REG_INFEX128 q_
#define REG_INFEX(reg_len) REG_INFEX##reg_len
#define POSTFIX_N(reg_len, data_len, data_type)	\
  CONCAT1 (REG_INFEX (reg_len), n_##data_type##data_len)
#define LANE_POSTFIX(reg_len, data_len, data_type) \
  CONCAT1 (REG_INFEX (reg_len),lane_##data_type##data_len)

/* Test values consist of bytes with following hex values.
   For example:
   TEST1 for int16_t will be 0xaaaa
   TEST1 for int32_t will be 0xaaaaaaaa
   etc.  */

#define TEST1h aa
#define TEST2h 55
#define TEST3h ff
#define TEST4h 00
#define TEST5h cc
#define TEST6h 33

#define TESTh_8(x) TEST##x##h
#define TESTh_16(x) CONCAT1 (TESTh_8 (x), TESTh_8 (x))
#define TESTh_32(x) CONCAT1 (TESTh_16 (x), TESTh_16 (x))
#define TESTh_64(x) CONCAT1 (TESTh_32 (x), TESTh_32 (x))

#define TEST_8(x) CONCAT1 (0x, TESTh_8 (x))
#define TEST_16(x) CONCAT1 (0x, TESTh_16 (x))
#define TEST_32(x) CONCAT1 (0x, TESTh_32 (x))
#define TEST_64(x) CONCAT1 (0x, TESTh_64 (x))

#define TEST(test, data_len) \
  CONCAT1 (TEST, _##data_len) (test)

#define GET_ELEMENT(reg_len, data_len, data_type)		\
  CONCAT1 (vget, LANE_POSTFIX (reg_len, data_len, data_type))

#define VMOV_INST(reg_len, data_len, data_type)			\
  CONCAT1 (vmov, POSTFIX_N (reg_len, data_len, data_type))

#define VMOV_OBSCURE_INST(reg_len, data_len, data_type)		\
  CONCAT1 (VMOV_INST (reg_len, data_len, data_type), _obscure)

#define RUN_TEST(reg_len, data_len, data_type,				\
		 test, n, a, b, c)					\
{									\
  int i;								\
  INHIB_OPTIMIZATION;							\
  (a) = TEST (test, data_len);						\
  INHIB_OPTIMIZATION;							\
  (b) = VMOV_OBSCURE_INST (reg_len, data_len, data_type) (&(a));	\
  (c) = TEST (test, data_len);						\
  for (i = 0; i < n; i++)						\
    {									\
      INHIB_OPTIMIZATION;						\
      a = GET_ELEMENT (reg_len, data_len, data_type) (b, i);		\
      if ((a) != (c))							\
	return 1;							\
    }									\
}

#define TYPE_f32 float32_t
#define TYPE_64_f32 float32x2_t
#define TYPE_128_f32 float32x4_t

#define TYPE_f64 float64_t
#define TYPE_64_f64 float64x1_t
#define TYPE_128_f64 float64x2_t

#define TYPE_s8 int8_t
#define TYPE_64_s8 int8x8_t
#define TYPE_128_s8 int8x16_t

#define TYPE_s16 int16_t
#define TYPE_64_s16 int16x4_t
#define TYPE_128_s16 int16x8_t

#define TYPE_s32 int32_t
#define TYPE_64_s32 int32x2_t
#define TYPE_128_s32 int32x4_t

#define TYPE_s64 int64_t
#define TYPE_64_s64 int64x1_t
#define TYPE_128_s64 int64x2_t

#define TYPE_u8 uint8_t
#define TYPE_64_u8 uint8x8_t
#define TYPE_128_u8 uint8x16_t

#define TYPE_u16 uint16_t
#define TYPE_64_u16 uint16x4_t
#define TYPE_128_u16 uint16x8_t

#define TYPE_u32 uint32_t
#define TYPE_64_u32 uint32x2_t
#define TYPE_128_u32 uint32x4_t

#define TYPE_u64 uint64_t
#define TYPE_64_u64 uint64x1_t
#define TYPE_128_u64 uint64x2_t

#define TYPE_p8 poly8_t
#define TYPE_64_p8 poly8x8_t
#define TYPE_128_p8 poly8x16_t

#define TYPE_p16 poly16_t
#define TYPE_64_p16 poly16x4_t
#define TYPE_128_p16 poly16x8_t

#define DIV64_8  8
#define DIV64_16 4
#define DIV64_32 2
#define DIV64_64 1

#define DIV128_8  16
#define DIV128_16 8
#define DIV128_32 4
#define DIV128_64 2

#define DIV(reg_len, data_len)			\
CONCAT1 (CONCAT1 (DIV, reg_len),		\
	 CONCAT1 (_, data_len))

#define VECTOR_TYPE(reg_len, data_len, data_type)	\
CONCAT1 (CONCAT1 (CONCAT1 (TYPE_,reg_len),		\
		  CONCAT1 (_,data_type)),		\
	 data_len)

#define SIMPLE_TYPE(data_len, data_type)	\
CONCAT1 (TYPE_,					\
	 CONCAT1 (data_type,			\
		  data_len))

#define OBSCURE_FUNC_NAME(reg_len, data_type, data_len)		\
CONCAT1 (CONCAT1 (vmov,						\
		  POSTFIX_N (reg_len, data_len, data_type)),	\
	 _obscure)

#define OBSCURE_FUNC(reg_len, data_len, data_type)	\
VECTOR_TYPE (reg_len, data_len, data_type)		\
__attribute__ ((noinline))				\
OBSCURE_FUNC_NAME (reg_len, data_type, data_len)	\
 (SIMPLE_TYPE (data_len, data_type) *ap)		\
{							\
  SIMPLE_TYPE (data_len, data_type) register a;		\
  INHIB_OPTIMIZATION;					\
  a = *ap;						\
  INHIB_OPTIMIZATION;					\
  return VMOV_INST (reg_len, data_len, data_type) (a);	\
}

#define TESTFUNC_NAME(reg_len, data_type, data_len)	\
CONCAT1 (test_vmov,					\
	 POSTFIX_N (reg_len, data_len, data_type))

#define TESTFUNC(reg_len, data_len, data_type)	\
int						\
TESTFUNC_NAME (reg_len, data_type, data_len) ()	\
{						\
  SIMPLE_TYPE (data_len, data_type) a;		\
  VECTOR_TYPE (reg_len, data_len, data_type) b;	\
  SIMPLE_TYPE (data_len, data_type) c;		\
						\
  RUN_TEST (reg_len, data_len, data_type, 1,	\
	    DIV (reg_len, data_len), a, b, c);	\
  RUN_TEST (reg_len, data_len, data_type, 2,	\
	    DIV (reg_len, data_len), a, b, c);	\
  RUN_TEST (reg_len, data_len, data_type, 3,	\
	    DIV (reg_len, data_len), a, b, c);	\
  RUN_TEST (reg_len, data_len, data_type, 4,	\
	    DIV (reg_len, data_len), a, b, c);	\
  RUN_TEST (reg_len, data_len, data_type, 5,	\
	    DIV (reg_len, data_len), a, b, c);	\
  RUN_TEST (reg_len, data_len, data_type, 6,	\
	    DIV (reg_len, data_len), a, b, c);	\
  return 0;					\
}

OBSCURE_FUNC (64, 32, f)
TESTFUNC (64, 32, f)
/* "dup  Vd.2s, Rn" is less preferable than "dup  Vd.2s, Vn.s[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.2s, w\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (64, 64, f)
TESTFUNC (64, 64, f)
/* "fmov  Dd, Rn" is generated instead of "dup  Dd, Rn".
   No assembley scan included.  */

OBSCURE_FUNC (64, 8, p)
TESTFUNC (64, 8, p)
/* Generates "dup  Vd.8b, Rn". Scan found near s8 version.  */

OBSCURE_FUNC (64, 16, p)
TESTFUNC (64, 16, p)
/* Generates "dup  Vd.4h, Rn". Scan found near s16 version.  */

OBSCURE_FUNC (64, 8, s)
TESTFUNC (64, 8, s)
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8b, w\[0-9\]+" 1 } } */

OBSCURE_FUNC (64, 16, s)
TESTFUNC (64, 16, s)
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4h, w\[0-9\]+" 1 } } */

OBSCURE_FUNC (64, 32, s)
TESTFUNC (64, 32, s)
/* "dup  Vd.2s, Rn" is less preferable than "dup  Vd.2s, Vn.s[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.2s, w\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (64, 64, s)
TESTFUNC (64, 64, s)
/* "fmov  Dd, Rn" is generated instead of "dup  Dd, Rn".
   No assembley scan included.  */

OBSCURE_FUNC (64, 8, u)
TESTFUNC (64, 8, u)
/* Generates "dup  Vd.8b, Rn". Scan found near s8 version.  */

OBSCURE_FUNC (64, 16, u)
TESTFUNC (64, 16, u)
/* Generates "dup  Vd.4h, Rn". Scan found near s16 version.  */

OBSCURE_FUNC (64, 32, u)
TESTFUNC (64, 32, u)
/* Generates "dup  Vd.2s, Rn". Scan found near s32 version.  */

OBSCURE_FUNC (64, 64, u)
TESTFUNC (64, 64, u)
/* "fmov  Dd, Rn" is generated instead of "dup  Dd, Rn".
   No assembley scan included.  */

OBSCURE_FUNC (128, 32, f)
TESTFUNC (128, 32, f)
/* "dup  Vd.4s, Rn" is less preferable than "dup  Vd.4s, Vn.s[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.4s, w\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (128, 64, f)
TESTFUNC (128, 64, f)
/* "dup  Vd.2d, Rn" is less preferable than "dup  Vd.2d, Vn.d[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.2d, x\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2d, v\[0-9\]+\.d\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (128, 8, p)
TESTFUNC (128, 8, p)
/* Generates "dup  Vd.16b, Rn". Scan found near s8 version.  */

OBSCURE_FUNC (128, 16, p)
TESTFUNC (128, 16, p)
/* Generates "dup  Vd.8h, Rn". Scan found near s16 version.  */

OBSCURE_FUNC (128, 8, s)
TESTFUNC (128, 8, s)
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.16b, w\[0-9\]+" 1 } } */

OBSCURE_FUNC (128, 16, s)
TESTFUNC (128, 16, s)
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8h, w\[0-9\]+" 1 } } */

OBSCURE_FUNC (128, 32, s)
TESTFUNC (128, 32, s)
/* "dup  Vd.4s, Rn" is less preferable than "dup  Vd.4s, Vn.s[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.4s, w\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (128, 64, s)
TESTFUNC (128, 64, s)
/* "dup  Vd.2d, Rn" is less preferable than "dup  Vd.2d, Vn.d[lane]".  */
/* { dg-final { scan-assembler-not "dup\\tv\[0-9\]+\.2d, x\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2d, v\[0-9\]+\.d\\\[\[0-9\]+\\\]" 3 } } */

OBSCURE_FUNC (128, 8, u)
TESTFUNC (128, 8, u)
/* Generates "dup  Vd.16b, Rn". Scan found near s8 version.  */

OBSCURE_FUNC (128, 16, u)
TESTFUNC (128, 16, u)
/* Generates "dup  Vd.8h, Rn". Scan found near s16 version.  */

OBSCURE_FUNC (128, 32, u)
TESTFUNC (128, 32, u)
/* Generates "dup  Vd.4s, Rn". Scan found near s32 version.  */

OBSCURE_FUNC (128, 64, u)
TESTFUNC (128, 64, u)
/* Generates "dup  Vd.2d, Rn". Scan found near s64 version.  */

int
main (int argc, char **argv)
{
  if (test_vmov_n_f32 ())
    abort ();
  if (test_vmov_n_f64 ())
    abort ();
  if (test_vmov_n_p8 ())
    abort ();
  if (test_vmov_n_p16 ())
    abort ();
  if (test_vmov_n_s8 ())
    abort ();
  if (test_vmov_n_s16 ())
    abort ();
  if (test_vmov_n_s32 ())
    abort ();
  if (test_vmov_n_s64 ())
    abort ();
  if (test_vmov_n_u8 ())
    abort ();
  if (test_vmov_n_u16 ())
    abort ();
  if (test_vmov_n_u32 ())
    abort ();
  if (test_vmov_n_u64 ())
    abort ();

  if (test_vmovq_n_f32 ())
    abort ();
  if (test_vmovq_n_f64 ())
    abort ();
  if (test_vmovq_n_p8 ())
    abort ();
  if (test_vmovq_n_p16 ())
    abort ();
  if (test_vmovq_n_s8 ())
    abort ();
  if (test_vmovq_n_s16 ())
    abort ();
  if (test_vmovq_n_s32 ())
    abort ();
  if (test_vmovq_n_s64 ())
    abort ();
  if (test_vmovq_n_u8 ())
    abort ();
  if (test_vmovq_n_u16 ())
    abort ();
  if (test_vmovq_n_u32 ())
    abort ();
  if (test_vmovq_n_u64 ())
    abort ();

  return 0;
}

