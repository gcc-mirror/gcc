// Test that ARM NEON vector types have their names mangled correctly. 

// { dg-do compile }
// { dg-require-effective-target arm_neon_ok }
// { dg-options "-mfpu=neon -mfloat-abi=softfp" }

#include <arm_neon.h>

void f0 (int8x8_t a) {}
void f1 (int16x4_t a) {}
void f2 (int32x2_t a) {}
void f3 (uint8x8_t a) {}
void f4 (uint16x4_t a) {}
void f5 (uint32x2_t a) {}
void f6 (float32x2_t a) {}
void f7 (poly8x8_t a) {}
void f8 (poly16x4_t a) {}

void f9 (int8x16_t a) {}
void f10 (int16x8_t a) {}
void f11 (int32x4_t a) {}
void f12 (uint8x16_t a) {}
void f13 (uint16x8_t a) {}
void f14 (uint32x4_t a) {}
void f15 (float32x4_t a) {}
void f16 (poly8x16_t a) {}
void f17 (poly16x8_t a) {}

void f18 (int8x16_t, int8x16_t) {}

// { dg-final { scan-assembler "_Z2f015__simd64_int8_t:" } }
// { dg-final { scan-assembler "_Z2f116__simd64_int16_t:" } }
// { dg-final { scan-assembler "_Z2f216__simd64_int32_t:" } }
// { dg-final { scan-assembler "_Z2f316__simd64_uint8_t:" } }
// { dg-final { scan-assembler "_Z2f417__simd64_uint16_t:" } }
// { dg-final { scan-assembler "_Z2f517__simd64_uint32_t:" } }
// { dg-final { scan-assembler "_Z2f618__simd64_float32_t:" } }
// { dg-final { scan-assembler "_Z2f716__simd64_poly8_t:" } }
// { dg-final { scan-assembler "_Z2f817__simd64_poly16_t:" } }
// { dg-final { scan-assembler "_Z2f916__simd128_int8_t:" } }
// { dg-final { scan-assembler "_Z3f1017__simd128_int16_t:" } }
// { dg-final { scan-assembler "_Z3f1117__simd128_int32_t:" } }
// { dg-final { scan-assembler "_Z3f1217__simd128_uint8_t:" } }
// { dg-final { scan-assembler "_Z3f1318__simd128_uint16_t:" } }
// { dg-final { scan-assembler "_Z3f1418__simd128_uint32_t:" } }
// { dg-final { scan-assembler "_Z3f1519__simd128_float32_t:" } }
// { dg-final { scan-assembler "_Z3f1617__simd128_poly8_t:" } }
// { dg-final { scan-assembler "_Z3f1718__simd128_poly16_t:" } }
// { dg-final { scan-assembler "_Z3f1816__simd128_int8_tS_:" } }
