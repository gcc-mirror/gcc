/* { dg-do compile } */

#include <arm_sve.h>

void f1(svbool_t) {}
void f2(svint8_t) {}
void f3(svint16_t) {}
void f4(svint32_t) {}
void f5(svint64_t) {}
void f6(svuint8_t) {}
void f7(svuint16_t) {}
void f8(svuint32_t) {}
void f9(svuint64_t) {}
void f10(svfloat16_t) {}
void f11(svfloat32_t) {}
void f12(svfloat64_t) {}
void f13(svbfloat16_t) {}

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
