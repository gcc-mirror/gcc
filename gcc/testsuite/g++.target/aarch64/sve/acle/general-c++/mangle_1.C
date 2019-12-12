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

/* { dg-final { scan-assembler "_Z2f110__SVBool_t:" } } */
/* { dg-final { scan-assembler "_Z2f210__SVInt8_t:" } } */
/* { dg-final { scan-assembler "_Z2f311__SVInt16_t:" } } */
/* { dg-final { scan-assembler "_Z2f411__SVInt32_t:" } } */
/* { dg-final { scan-assembler "_Z2f511__SVInt64_t:" } } */
/* { dg-final { scan-assembler "_Z2f611__SVUint8_t:" } } */
/* { dg-final { scan-assembler "_Z2f712__SVUint16_t:" } } */
/* { dg-final { scan-assembler "_Z2f812__SVUint32_t:" } } */
/* { dg-final { scan-assembler "_Z2f912__SVUint64_t:" } } */
/* { dg-final { scan-assembler "_Z3f1013__SVFloat16_t:" } } */
/* { dg-final { scan-assembler "_Z3f1113__SVFloat32_t:" } } */
/* { dg-final { scan-assembler "_Z3f1213__SVFloat64_t:" } } */
