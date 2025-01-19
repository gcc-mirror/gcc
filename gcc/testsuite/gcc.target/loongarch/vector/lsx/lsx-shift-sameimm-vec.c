/* Test shift bits overflow in vector */
/* { dg-do assemble } */
/* { dg-options "-mlsx -O2 -save-temps" } */
/* { dg-final { scan-assembler "vslli.b.*,1" } } */
/* { dg-final { scan-assembler "vslli.b.*,7" } } */
/* { dg-final { scan-assembler "vslli.h.*,1" } } */
/* { dg-final { scan-assembler "vslli.h.*,15" } } */
/* { dg-final { scan-assembler "vslli.w.*,1" } } */
/* { dg-final { scan-assembler "vslli.w.*,31" } } */
/* { dg-final { scan-assembler "vslli.d.*,1" } } */
/* { dg-final { scan-assembler "vslli.d.*,63" } } */
/* { dg-final { scan-assembler "vsrli.b.*,1" } } */
/* { dg-final { scan-assembler "vsrli.b.*,7" } } */
/* { dg-final { scan-assembler "vsrli.h.*,1" } } */
/* { dg-final { scan-assembler "vsrli.h.*,15" } } */
/* { dg-final { scan-assembler "vsrli.w.*,1" } } */
/* { dg-final { scan-assembler "vsrli.w.*,31" } } */
/* { dg-final { scan-assembler "vsrli.d.*,1" } } */
/* { dg-final { scan-assembler "vsrli.d.*,63" } } */
/* { dg-final { scan-assembler "vsrai.b.*,1" } } */
/* { dg-final { scan-assembler "vsrai.b.*,7" } } */
/* { dg-final { scan-assembler "vsrai.h.*,1" } } */
/* { dg-final { scan-assembler "vsrai.h.*,15" } } */
/* { dg-final { scan-assembler "vsrai.w.*,1" } } */
/* { dg-final { scan-assembler "vsrai.w.*,31" } } */
/* { dg-final { scan-assembler "vsrai.d.*,1" } } */
/* { dg-final { scan-assembler "vsrai.d.*,63" } } */

typedef signed char v16i8 __attribute__ ((vector_size (16), aligned (16)));
typedef short v8i16 __attribute__ ((vector_size (16), aligned (16)));
typedef int v4i32 __attribute__ ((vector_size (16), aligned (16)));
typedef long long v2i64 __attribute__ ((vector_size (16), aligned (16)));

#define TWICE(_) _, _
#define V16I8_RVAL(_) (v16i8) {TWICE(TWICE(TWICE(TWICE(_))))}
#define V8I16_RVAL(_) (v8i16) {TWICE(TWICE(TWICE(_)))}
#define V4I32_RVAL(_) (v4i32) {TWICE(TWICE(_))}
#define V2I64_RVAL(_) (v2i64) {TWICE(_)}

#define TEST_FUNC(typ, key, inst, rept, val)    \
typ                                             \
_##key##inst (typ _)                            \
{                                               \
  return __builtin_lsx_##inst(_, rept(val));    \
}

TEST_FUNC(v16i8, pos, vsll_b, V16I8_RVAL, 65)
TEST_FUNC(v16i8, neg, vsll_b, V16I8_RVAL, -65)
TEST_FUNC(v8i16, pos, vsll_h, V8I16_RVAL, 65)
TEST_FUNC(v8i16, neg, vsll_h, V8I16_RVAL, -65)
TEST_FUNC(v4i32, pos, vsll_w, V4I32_RVAL, 65)
TEST_FUNC(v4i32, neg, vsll_w, V4I32_RVAL, -65)
TEST_FUNC(v2i64, pos, vsll_d, V2I64_RVAL, 65)
TEST_FUNC(v2i64, neg, vsll_d, V2I64_RVAL, -65)

TEST_FUNC(v16i8, pos, vsrl_b, V16I8_RVAL, 65)
TEST_FUNC(v16i8, neg, vsrl_b, V16I8_RVAL, -65)
TEST_FUNC(v8i16, pos, vsrl_h, V8I16_RVAL, 65)
TEST_FUNC(v8i16, neg, vsrl_h, V8I16_RVAL, -65)
TEST_FUNC(v4i32, pos, vsrl_w, V4I32_RVAL, 65)
TEST_FUNC(v4i32, neg, vsrl_w, V4I32_RVAL, -65)
TEST_FUNC(v2i64, pos, vsrl_d, V2I64_RVAL, 65)
TEST_FUNC(v2i64, neg, vsrl_d, V2I64_RVAL, -65)

TEST_FUNC(v16i8, pos, vsra_b, V16I8_RVAL, 65)
TEST_FUNC(v16i8, neg, vsra_b, V16I8_RVAL, -65)
TEST_FUNC(v8i16, pos, vsra_h, V8I16_RVAL, 65)
TEST_FUNC(v8i16, neg, vsra_h, V8I16_RVAL, -65)
TEST_FUNC(v4i32, pos, vsra_w, V4I32_RVAL, 65)
TEST_FUNC(v4i32, neg, vsra_w, V4I32_RVAL, -65)
TEST_FUNC(v2i64, pos, vsra_d, V2I64_RVAL, 65)
TEST_FUNC(v2i64, neg, vsra_d, V2I64_RVAL, -65)
