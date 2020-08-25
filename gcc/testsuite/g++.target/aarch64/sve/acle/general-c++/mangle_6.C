/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-additional-options "-msve-vector-bits=128" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))

void f1(svbool_t FIXED_ATTR) {}
void f2(svint8_t FIXED_ATTR) {}
void f3(svint16_t FIXED_ATTR) {}
void f4(svint32_t FIXED_ATTR) {}
void f5(svint64_t FIXED_ATTR) {}
void f6(svuint8_t FIXED_ATTR) {}
void f7(svuint16_t FIXED_ATTR) {}
void f8(svuint32_t FIXED_ATTR) {}
void f9(svuint64_t FIXED_ATTR) {}
void f10(svfloat16_t FIXED_ATTR) {}
void f11(svfloat32_t FIXED_ATTR) {}
void f12(svfloat64_t FIXED_ATTR) {}
void f13(svbfloat16_t FIXED_ATTR) {}

/* { dg-final { scan-assembler "_Z2f19__SVE_VLSIu10__SVBool_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f29__SVE_VLSIu10__SVInt8_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f39__SVE_VLSIu11__SVInt16_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f49__SVE_VLSIu11__SVInt32_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f59__SVE_VLSIu11__SVInt64_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f69__SVE_VLSIu11__SVUint8_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f79__SVE_VLSIu12__SVUint16_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f89__SVE_VLSIu12__SVUint32_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z2f99__SVE_VLSIu12__SVUint64_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z3f109__SVE_VLSIu13__SVFloat16_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z3f119__SVE_VLSIu13__SVFloat32_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z3f129__SVE_VLSIu13__SVFloat64_tLj128EE:" } } */
/* { dg-final { scan-assembler "_Z3f139__SVE_VLSIu14__SVBfloat16_tLj128EE:" } } */
