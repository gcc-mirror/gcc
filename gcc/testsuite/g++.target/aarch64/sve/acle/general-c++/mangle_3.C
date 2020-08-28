/* { dg-do compile } */
/* { dg-additional-options "-msve-vector-bits=256" } */

#include <arm_sve.h>

typedef __SVInt8_t t1;
typedef svint8_t t2;
/* Distinct from svint8_t, but compatible with it.  */
typedef int8_t t3 __attribute__((vector_size(32)));

void f1(t1) {}
void f2(t2) {}
void f3(t3) {}
void f4(t1 &a, t2 &b, t3 &c) { a = b = c; }

/* { dg-final { scan-assembler "_Z2f1u10__SVInt8_t:" } } */
/* { dg-final { scan-assembler "_Z2f2u10__SVInt8_t:" } } */
/* { dg-final { scan-assembler "_Z2f3Dv32_a:" } } */
