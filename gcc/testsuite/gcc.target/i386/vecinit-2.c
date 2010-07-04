/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
#define vector __attribute__((vector_size(16)))

int a;
vector int f1(void) { return (vector int){ a, 0, 0, 0}; }
vector int f2(void) { return (vector int){ 0, a, 0, 0}; }
vector int f3(void) { return (vector int){ 0, 0, a, 0}; }
vector int f4(void) { return (vector int){ 0, 0, 0, a}; }
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "xor" } } */
/* { dg-final { scan-assembler-not "%mm" } } */
