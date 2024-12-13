/* Test shift bits overflow in vector */
/* { dg-do assemble } */
/* { dg-options "-mlasx -O2 -save-temps" } */
/* { dg-final { scan-assembler "xvslli.b.*,1" } } */
/* { dg-final { scan-assembler "xvslli.b.*,7" } } */
/* { dg-final { scan-assembler "xvslli.h.*,1" } } */
/* { dg-final { scan-assembler "xvslli.h.*,15" } } */
/* { dg-final { scan-assembler "xvslli.w.*,1" } } */
/* { dg-final { scan-assembler "xvslli.w.*,31" } } */
/* { dg-final { scan-assembler "xvslli.d.*,1" } } */
/* { dg-final { scan-assembler "xvslli.d.*,63" } } */
/* { dg-final { scan-assembler "xvsrli.b.*,1" } } */
/* { dg-final { scan-assembler "xvsrli.b.*,7" } } */
/* { dg-final { scan-assembler "xvsrli.h.*,1" } } */
/* { dg-final { scan-assembler "xvsrli.h.*,15" } } */
/* { dg-final { scan-assembler "xvsrli.w.*,1" } } */
/* { dg-final { scan-assembler "xvsrli.w.*,31" } } */
/* { dg-final { scan-assembler "xvsrli.d.*,1" } } */
/* { dg-final { scan-assembler "xvsrli.d.*,63" } } */
/* { dg-final { scan-assembler "xvsrai.b.*,1" } } */
/* { dg-final { scan-assembler "xvsrai.b.*,7" } } */
/* { dg-final { scan-assembler "xvsrai.h.*,1" } } */
/* { dg-final { scan-assembler "xvsrai.h.*,15" } } */
/* { dg-final { scan-assembler "xvsrai.w.*,1" } } */
/* { dg-final { scan-assembler "xvsrai.w.*,31" } } */
/* { dg-final { scan-assembler "xvsrai.d.*,1" } } */
/* { dg-final { scan-assembler "xvsrai.d.*,63" } } */

typedef signed char v32i8 __attribute__ ((vector_size (32), aligned (32)));
typedef short v16i16 __attribute__ ((vector_size (32), aligned (32)));
typedef int v8i32 __attribute__ ((vector_size (32), aligned (32)));
typedef long long v4i64 __attribute__ ((vector_size (32), aligned (32)));

#define TWICE(_) _, _
#define V32I8_RVAL(_) (v32i8)   {TWICE(TWICE(TWICE(TWICE(TWICE(_)))))}
#define V16I16_RVAL(_) (v16i16) {TWICE(TWICE(TWICE(TWICE(_))))}
#define V8I32_RVAL(_) (v8i32)   {TWICE(TWICE(TWICE(_)))}
#define V4I64_RVAL(_) (v4i64)   {TWICE(TWICE(_))}

#define TEST_FUNC(typ, key, inst, rept, val)    \
typ                                             \
_##key##inst (typ _)                            \
{                                               \
  return __builtin_lasx_##inst(_, rept(val));   \
}

TEST_FUNC(v32i8, pos, xvsll_b, V32I8_RVAL, 65)
TEST_FUNC(v32i8, neg, xvsll_b, V32I8_RVAL, -65)
TEST_FUNC(v16i16, pos, xvsll_h, V16I16_RVAL, 65)
TEST_FUNC(v16i16, neg, xvsll_h, V16I16_RVAL, -65)
TEST_FUNC(v8i32, pos, xvsll_w, V8I32_RVAL, 65)
TEST_FUNC(v8i32, neg, xvsll_w, V8I32_RVAL, -65)
TEST_FUNC(v4i64, pos, xvsll_d, V4I64_RVAL, 65)
TEST_FUNC(v4i64, neg, xvsll_d, V4I64_RVAL, -65)

TEST_FUNC(v32i8, pos, xvsrl_b, V32I8_RVAL, 65)
TEST_FUNC(v32i8, neg, xvsrl_b, V32I8_RVAL, -65)
TEST_FUNC(v16i16, pos, xvsrl_h, V16I16_RVAL, 65)
TEST_FUNC(v16i16, neg, xvsrl_h, V16I16_RVAL, -65)
TEST_FUNC(v8i32, pos, xvsrl_w, V8I32_RVAL, 65)
TEST_FUNC(v8i32, neg, xvsrl_w, V8I32_RVAL, -65)
TEST_FUNC(v4i64, pos, xvsrl_d, V4I64_RVAL, 65)
TEST_FUNC(v4i64, neg, xvsrl_d, V4I64_RVAL, -65)

TEST_FUNC(v32i8, pos, xvsra_b, V32I8_RVAL, 65)
TEST_FUNC(v32i8, neg, xvsra_b, V32I8_RVAL, -65)
TEST_FUNC(v16i16, pos, xvsra_h, V16I16_RVAL, 65)
TEST_FUNC(v16i16, neg, xvsra_h, V16I16_RVAL, -65)
TEST_FUNC(v8i32, pos, xvsra_w, V8I32_RVAL, 65)
TEST_FUNC(v8i32, neg, xvsra_w, V8I32_RVAL, -65)
TEST_FUNC(v4i64, pos, xvsra_d, V4I64_RVAL, 65)
TEST_FUNC(v4i64, neg, xvsra_d, V4I64_RVAL, -65)
