/* { dg-do compile } */
/* { dg-options "-O2 -march=k8 -msse2" } */

#define vector __attribute__((vector_size(16)))

float a;
vector float f1(void) { return (vector float){ a, 0.0, 0.0, 0.0}; }
vector float f2(void) { return (vector float){ 0.0, a, 0.0, 0.0}; }
vector float f3(void) { return (vector float){ 0.0, 0.0, a, 0.0}; }
vector float f4(void) { return (vector float){ 0.0, 0.0, 0.0, a}; }
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "xor" } } */
/* { dg-final { scan-assembler-not "%mm" } } */
