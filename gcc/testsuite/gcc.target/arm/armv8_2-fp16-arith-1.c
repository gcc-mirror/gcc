/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok }  */
/* { dg-options "-O2 -ffast-math" }  */
/* { dg-add-options arm_v8_2a_fp16_neon }  */

/* Test instructions generated for half-precision arithmetic with
   unsafe-math-optimizations enabled.  */

typedef __fp16 float16_t;
typedef __simd64_float16_t float16x4_t;
typedef __simd128_float16_t float16x8_t;

typedef short int16x4_t __attribute__ ((vector_size (8)));
typedef short int int16x8_t  __attribute__ ((vector_size (16)));

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

/* Vectors of size 4.  */

TEST_UNOP (neg, -, float16x4_t)

TEST_BINOP (add, +, float16x4_t)
TEST_BINOP (sub, -, float16x4_t)
TEST_BINOP (mult, *, float16x4_t)
TEST_BINOP (div, /, float16x4_t)

TEST_CMP (equal, ==, int16x4_t, float16x4_t)
TEST_CMP (unequal, !=, int16x4_t, float16x4_t)
TEST_CMP (lessthan, <, int16x4_t, float16x4_t)
TEST_CMP (greaterthan, >, int16x4_t, float16x4_t)
TEST_CMP (lessthanequal, <=, int16x4_t, float16x4_t)
TEST_CMP (greaterthanqual, >=, int16x4_t, float16x4_t)

/* Vectors of size 8.  */

TEST_UNOP (neg, -, float16x8_t)

TEST_BINOP (add, +, float16x8_t)
TEST_BINOP (sub, -, float16x8_t)
TEST_BINOP (mult, *, float16x8_t)
TEST_BINOP (div, /, float16x8_t)

TEST_CMP (equal, ==, int16x8_t, float16x8_t)
TEST_CMP (unequal, !=, int16x8_t, float16x8_t)
TEST_CMP (lessthan, <, int16x8_t, float16x8_t)
TEST_CMP (greaterthan, >, int16x8_t, float16x8_t)
TEST_CMP (lessthanequal, <=, int16x8_t, float16x8_t)
TEST_CMP (greaterthanqual, >=, int16x8_t, float16x8_t)

/* { dg-final { scan-assembler-times {vneg\.f16\ts[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vneg\.f16\td[0-9]+, d[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vneg\.f16\tq[0-9]+, q[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vabs\.f16\ts[0-9]+, s[0-9]+} 2 } }  */

/* { dg-final { scan-assembler-times {vadd\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vadd\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vadd\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

/* { dg-final { scan-assembler-times {vsub\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vsub\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vsub\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

/* { dg-final { scan-assembler-times {vmul\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vmul\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 1 } }  */
/* { dg-final { scan-assembler-times {vmul\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

/* { dg-final { scan-assembler-times {vdiv\.f16\ts[0-9]+, s[0-9]+, s[0-9]+} 13 } }  */

/* For float16_t.  */
/* { dg-final { scan-assembler-times {vcmp\.f32\ts[0-9]+, s[0-9]+} 6 } }  */

/* For float16x4_t.  */
/* { dg-final { scan-assembler-times {vceq\.f16\td[0-9]+, d[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcge\.f16\td[0-9]+, d[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcgt\.f16\td[0-9]+, d[0-9]+} 2 } }  */

/* For float16x8_t.  */
/* { dg-final { scan-assembler-times {vceq\.f16\tq[0-9]+, q[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcge\.f16\tq[0-9]+, q[0-9]+} 2 } }  */
/* { dg-final { scan-assembler-times {vcgt\.f16\tq[0-9]+, q[0-9]+} 2 } }  */

/* { dg-final { scan-assembler-not {vadd\.f32} } }  */
/* { dg-final { scan-assembler-not {vsub\.f32} } }  */
/* { dg-final { scan-assembler-not {vmul\.f32} } }  */
/* { dg-final { scan-assembler-not {vdiv\.f32} } }  */
/* { dg-final { scan-assembler-not {vcmp\.f16} } }  */
/* { dg-final { scan-assembler-not {vcmpe\.f16} } }  */
