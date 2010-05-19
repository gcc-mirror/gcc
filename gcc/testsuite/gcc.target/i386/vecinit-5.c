/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
#define vector __attribute__((vector_size(16)))

float a, b;
vector float f1(void) { return (vector float){ 0.0, 0.0, a, a}; }
vector float f2(void) { return (vector float){ a, a, 0.0, 0.0}; }
vector float f3(void) { return (vector float){ 0.0, a, 0.0, a}; }
vector float f4(void) { return (vector float){ a, 0.0, a, 0.0}; }

vector float f5(void) { return (vector float){ 1.0, 1.0, a, a}; }
vector float f6(void) { return (vector float){ a, a, 1.0, 1.0}; }
vector float f7(void) { return (vector float){ 1.0, a, 1.0, a}; }
vector float f8(void) { return (vector float){ a, 1.0, a, 1.0}; }

vector float fa(void) { return (vector float){ 1.0, 1.0, 0.0, 0.0}; }
vector float fb(void) { return (vector float){ 1.0, 0.0, 1.0, 0.0}; }
vector float fc(void) { return (vector float){ 0.0, 1.0, 0.0, 1.0}; }

vector float fA(void) { return (vector float){ a, a, b, b}; }
vector float fB(void) { return (vector float){ a, b, a, b}; }
vector float fC(void) { return (vector float){ a, a, a, a}; }

/* { dg-final { scan-assembler-not "%mm" } } */
