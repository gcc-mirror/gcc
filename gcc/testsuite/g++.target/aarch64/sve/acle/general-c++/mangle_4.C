/* { dg-do compile } */

#include <arm_sve.h>

void f1(svint8x2_t) {}
void f2(svint16x2_t) {}
void f3(svint32x2_t) {}
void f4(svint64x2_t) {}
void f5(svuint8x2_t) {}
void f6(svuint16x2_t) {}
void f7(svuint32x2_t) {}
void f8(svuint64x2_t) {}
void f9(svfloat16x2_t) {}
void f10(svfloat32x2_t) {}
void f11(svfloat64x2_t) {}

void g1(svint8x3_t) {}
void g2(svint16x3_t) {}
void g3(svint32x3_t) {}
void g4(svint64x3_t) {}
void g5(svuint8x3_t) {}
void g6(svuint16x3_t) {}
void g7(svuint32x3_t) {}
void g8(svuint64x3_t) {}
void g9(svfloat16x3_t) {}
void g10(svfloat32x3_t) {}
void g11(svfloat64x3_t) {}

void h1(svint8x4_t) {}
void h2(svint16x4_t) {}
void h3(svint32x4_t) {}
void h4(svint64x4_t) {}
void h5(svuint8x4_t) {}
void h6(svuint16x4_t) {}
void h7(svuint32x4_t) {}
void h8(svuint64x4_t) {}
void h9(svfloat16x4_t) {}
void h10(svfloat32x4_t) {}
void h11(svfloat64x4_t) {}

/* { dg-final { scan-assembler "_Z2f110svint8x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f211svint16x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f311svint32x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f411svint64x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f511svuint8x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f612svuint16x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f712svuint32x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f812svuint64x2_t:" } } */
/* { dg-final { scan-assembler "_Z2f913svfloat16x2_t:" } } */
/* { dg-final { scan-assembler "_Z3f1013svfloat32x2_t:" } } */
/* { dg-final { scan-assembler "_Z3f1113svfloat64x2_t:" } } */

/* { dg-final { scan-assembler "_Z2g110svint8x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g211svint16x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g311svint32x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g411svint64x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g511svuint8x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g612svuint16x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g712svuint32x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g812svuint64x3_t:" } } */
/* { dg-final { scan-assembler "_Z2g913svfloat16x3_t:" } } */
/* { dg-final { scan-assembler "_Z3g1013svfloat32x3_t:" } } */
/* { dg-final { scan-assembler "_Z3g1113svfloat64x3_t:" } } */

/* { dg-final { scan-assembler "_Z2h110svint8x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h211svint16x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h311svint32x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h411svint64x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h511svuint8x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h612svuint16x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h712svuint32x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h812svuint64x4_t:" } } */
/* { dg-final { scan-assembler "_Z2h913svfloat16x4_t:" } } */
/* { dg-final { scan-assembler "_Z3h1013svfloat32x4_t:" } } */
/* { dg-final { scan-assembler "_Z3h1113svfloat64x4_t:" } } */
