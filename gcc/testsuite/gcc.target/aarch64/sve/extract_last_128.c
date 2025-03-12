/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=128" } */

#include <arm_sve.h>

#define TEST(TYPE, TY)			\
  TYPE extract_last_##TY (sv##TYPE x)	\
  {					\
    svbool_t pg = svpfalse ();		\
    return svlastb_##TY (pg, x);	\
  }

TEST(bfloat16_t, bf16)
TEST(float16_t, f16)
TEST(float32_t, f32)
TEST(float64_t, f64)
TEST(int8_t, s8)
TEST(int16_t, s16)
TEST(int32_t, s32)
TEST(int64_t, s64)
TEST(uint8_t, u8)
TEST(uint16_t, u16)
TEST(uint32_t, u32)
TEST(uint64_t, u64)

/* { dg-final { scan-assembler-times {\tdup\th0, v0\.h\[7\]} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\ts0, v0\.s\[3\]} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\td0, v0\.d\[1\]} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw0, v0\.h\[7\]} 2 } } */
/* { dg-final { scan-assembler-times {\tumov\tw0, v0\.b\[15\]} 2 } } */
/* { dg-final { scan-assembler-times {\tumov\tw0, v0\.s\[3\]} 2 } } */
/* { dg-final { scan-assembler-times {\tumov\tx0, v0\.d\[1\]} 2 } } */
/* { dg-final { scan-assembler-not "lastb" } } */