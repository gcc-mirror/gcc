/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
#define vector __attribute__((vector_size(16)))

int a, b;
vector int f1(void) { return (vector int){ 0, 0, a, a}; }
vector int f2(void) { return (vector int){ a, a, 0, 0}; }
vector int f3(void) { return (vector int){ 0, a, 0, a}; }
vector int f4(void) { return (vector int){ a, 0, a, 0}; }

vector int f5(void) { return (vector int){ 1, 1, a, a}; }
vector int f6(void) { return (vector int){ a, a, 1, 1}; }
vector int f7(void) { return (vector int){ 1, a, 1, a}; }
vector int f8(void) { return (vector int){ a, 1, a, 1}; }

vector int fa(void) { return (vector int){ 1, 1, 0, 0}; }
vector int fb(void) { return (vector int){ 1, 0, 1, 0}; }
vector int fc(void) { return (vector int){ 0, 1, 0, 1}; }

vector int fA(void) { return (vector int){ a, a, b, b}; }
vector int fB(void) { return (vector int){ a, b, a, b}; }
vector int fC(void) { return (vector int){ a, a, a, a}; }

/* { dg-final { scan-assembler-not "%mm" } } */
