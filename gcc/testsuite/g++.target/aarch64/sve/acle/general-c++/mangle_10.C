/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-additional-options "-msve-vector-bits=2048" } */

#include "mangle_6.C"

/* { dg-final { scan-assembler "_Z2f19__SVE_VLSIu10__SVBool_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f29__SVE_VLSIu10__SVInt8_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f39__SVE_VLSIu11__SVInt16_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f49__SVE_VLSIu11__SVInt32_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f59__SVE_VLSIu11__SVInt64_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f69__SVE_VLSIu11__SVUint8_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f79__SVE_VLSIu12__SVUint16_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f89__SVE_VLSIu12__SVUint32_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z2f99__SVE_VLSIu12__SVUint64_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z3f109__SVE_VLSIu13__SVFloat16_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z3f119__SVE_VLSIu13__SVFloat32_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z3f129__SVE_VLSIu13__SVFloat64_tLj2048EE:" } } */
/* { dg-final { scan-assembler "_Z3f139__SVE_VLSIu14__SVBfloat16_tLj2048EE:" } } */
