/* { dg-do compile } */

void f1(__SVBool_t) {}
void f2(__SVInt8_t) {}
void f3(__SVInt16_t) {}
void f4(__SVInt32_t) {}
void f5(__SVInt64_t) {}
void f6(__SVUint8_t) {}
void f7(__SVUint16_t) {}
void f8(__SVUint32_t) {}
void f9(__SVUint64_t) {}
void f10(__SVFloat16_t) {}
void f11(__SVFloat32_t) {}
void f12(__SVFloat64_t) {}
void f13(__SVBfloat16_t) {}
void f14(__SVCount_t) {}

/* { dg-final { scan-assembler "_Z2f1u10__SVBool_t:" } } */
/* { dg-final { scan-assembler "_Z2f2u10__SVInt8_t:" } } */
/* { dg-final { scan-assembler "_Z2f3u11__SVInt16_t:" } } */
/* { dg-final { scan-assembler "_Z2f4u11__SVInt32_t:" } } */
/* { dg-final { scan-assembler "_Z2f5u11__SVInt64_t:" } } */
/* { dg-final { scan-assembler "_Z2f6u11__SVUint8_t:" } } */
/* { dg-final { scan-assembler "_Z2f7u12__SVUint16_t:" } } */
/* { dg-final { scan-assembler "_Z2f8u12__SVUint32_t:" } } */
/* { dg-final { scan-assembler "_Z2f9u12__SVUint64_t:" } } */
/* { dg-final { scan-assembler "_Z3f10u13__SVFloat16_t:" } } */
/* { dg-final { scan-assembler "_Z3f11u13__SVFloat32_t:" } } */
/* { dg-final { scan-assembler "_Z3f12u13__SVFloat64_t:" } } */
/* { dg-final { scan-assembler "_Z3f13u14__SVBfloat16_t:" } } */
/* { dg-final { scan-assembler "_Z3f14u11__SVCount_t:" } } */
