/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2 -ffast-math" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

/* Test instructions generated for half-precision arithmetic.  */

typedef __fp16 float16_t;
typedef __simd64_float16_t float16x4_t;
typedef __simd128_float16_t float16x8_t;

float16_t
fp16_abs (float16_t a)
{
  return (a < 0) ? -a : a;
}

#define TEST_UNOP(NAME, OPERATOR, TY)		\
  TY test_##NAME##_##TY (TY a)			\
  {						\
    return OPERATOR (a);			\
  }

#define TEST_BINOP(NAME, OPERATOR, TY)		\
  TY test_##NAME##_##TY (TY a, TY b)		\
  {						\
    return a OPERATOR b;			\
  }

#define TEST_CMP(NAME, OPERATOR, RTY, TY)	\
  RTY test_##NAME##_##TY (TY a, TY b)		\
  {						\
    return a OPERATOR b;			\
  }

/* Scalars.  */

TEST_UNOP (neg, -, float16_t)
TEST_UNOP (abs, fp16_abs, float16_t)

TEST_BINOP (add, +, float16_t)
TEST_BINOP (sub, -, float16_t)
TEST_BINOP (mult, *, float16_t)
TEST_BINOP (div, /, float16_t)

TEST_CMP (equal, ==, int, float16_t)
TEST_CMP (unequal, !=, int, float16_t)
TEST_CMP (lessthan, <, int, float16_t)
TEST_CMP (greaterthan, >, int, float16_t)
TEST_CMP (lessthanequal, <=, int, float16_t)
TEST_CMP (greaterthanqual, >=, int, float16_t)

/* { dg-final { scan-assembler-times {vneg\.f16\ts[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vabs\.f16\ts[0-9]+, s[0-9]+} 2 } }  */

/* { dg-final { scan-assembler-times {vadd\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vsub\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vmul\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vdiv\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vcmp\.f32\ts[0-9]+, s[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcmpe\.f32\ts[0-9]+, s[0-9]+} 4 } }  */

/* { dg-final { scan-assembler-not {vadd\.f32} } }  */
/* { dg-final { scan-assembler-not {vsub\.f32} } }  */
/* { dg-final { scan-assembler-not {vmul\.f32} } }  */
/* { dg-final { scan-assembler-not {vdiv\.f32} } }  */
/* { dg-final { scan-assembler-not {vcmp\.f16} } }  */
/* { dg-final { scan-assembler-not {vcmpe\.f16} } }  */
