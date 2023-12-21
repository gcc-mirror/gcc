// Test that AArch64 AdvSIMD (NEON) vector types have their names mangled
// correctly.

// { dg-do compile { target { aarch64*-*-* } } }
// { dg-additional-options -fabi-compat-version=0 }

#include <arm_neon.h>

void f0 (int8x8_t a) {}
void f1 (int16x4_t a) {}
void f2 (int32x2_t a) {}
void f22 (int64x1_t a) {}
void f3 (uint8x8_t a) {}
void f4 (uint16x4_t a) {}
void f5 (uint32x2_t a) {}
void f23 (uint64x1_t a) {}
void f61 (float16x4_t a) {}
void f62 (bfloat16x4_t a) {}
void f6 (float32x2_t a) {}
void f7 (poly8x8_t a) {}
void f8 (poly16x4_t a) {}

void f9 (int8x16_t a) {}
void f10 (int16x8_t a) {}
void f11 (int32x4_t a) {}
void f12 (int64x2_t a) {}
void f13 (uint8x16_t a) {}
void f14 (uint16x8_t a) {}
void f15 (uint32x4_t a) {}
void f16 (uint64x2_t a) {}
void f171 (float16x8_t a) {}
void f172 (bfloat16x8_t a) {}
void f17 (float32x4_t a) {}
void f18 (float64x2_t a) {}
void f19 (poly8x16_t a) {}
void f20 (poly16x8_t a) {}
void f21 (float64x1_t a) {}

void g1 (int8x16_t, int8x16_t) {}


// { dg-final { scan-assembler "_Z2f010__Int8x8_t:" } }
// { dg-final { scan-assembler "_Z2f111__Int16x4_t:" } }
// { dg-final { scan-assembler "_Z2f211__Int32x2_t:" } }
// { dg-final { scan-assembler "_Z3f2211__Int64x1_t:" } }
// { dg-final { scan-assembler "_Z2f311__Uint8x8_t:" } }
// { dg-final { scan-assembler "_Z2f412__Uint16x4_t:" } }
// { dg-final { scan-assembler "_Z2f512__Uint32x2_t:" } }
// { dg-final { scan-assembler "_Z3f2312__Uint64x1_t:" } }
// { dg-final { scan-assembler "_Z3f6113__Float16x4_t:" } }
// { dg-final { scan-assembler "_Z3f6214__Bfloat16x4_t:" } }
// { dg-final { scan-assembler "_Z2f613__Float32x2_t:" } }
// { dg-final { scan-assembler "_Z2f711__Poly8x8_t:" } }
// { dg-final { scan-assembler "_Z2f812__Poly16x4_t:" } }
// { dg-final { scan-assembler "_Z2f911__Int8x16_t:" } }
// { dg-final { scan-assembler "_Z3f1011__Int16x8_t:" } }
// { dg-final { scan-assembler "_Z3f1111__Int32x4_t:" } }
// { dg-final { scan-assembler "_Z3f1211__Int64x2_t:" } }
// { dg-final { scan-assembler "_Z3f1312__Uint8x16_t:" } }
// { dg-final { scan-assembler "_Z3f1412__Uint16x8_t:" } }
// { dg-final { scan-assembler "_Z3f1512__Uint32x4_t:" } }
// { dg-final { scan-assembler "_Z3f1612__Uint64x2_t:" } }
// { dg-final { scan-assembler "_Z4f17113__Float16x8_t:" } }
// { dg-final { scan-assembler "_Z4f17214__Bfloat16x8_t:" } }
// { dg-final { scan-assembler "_Z3f1713__Float32x4_t:" } }
// { dg-final { scan-assembler "_Z3f1813__Float64x2_t:" } }
// { dg-final { scan-assembler "_Z3f1912__Poly8x16_t:" } }
// { dg-final { scan-assembler "_Z3f2012__Poly16x8_t:" } }
// { dg-final { scan-assembler "_Z3f2113__Float64x1_t:" } }
// { dg-final { scan-assembler "_Z2g111__Int8x16_tS_:" } }
