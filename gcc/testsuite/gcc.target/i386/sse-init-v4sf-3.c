/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse -mno-sse2" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

float m,n;

v4sf fa000(float a) { return (v4sf){a,0.0f,0.0f,0.0f}; }
v4sf f0a00(float a) { return (v4sf){0.0f,a,0.0f,0.0f}; }
v4sf f00a0(float a) { return (v4sf){0.0f,0.0f,a,0.0f}; }
v4sf f000a(float a) { return (v4sf){0.0f,0.0f,0.0f,a}; }

v4sf faa00(float a) { return (v4sf){a,a,0.0f,0.0f}; }
v4sf fa0a0(float a) { return (v4sf){a,0.0f,a,0.0f}; }

v4sf faaaa(float a) { return (v4sf){a,a,a,a}; }

v4sf fab00(float a, float b) { return (v4sf){a,b,0.0f,0.0f}; }
v4sf fa0b0(float a, float b) { return (v4sf){a,0.0f,b,0.0f}; }
v4sf fabab(float a, float b) { return (v4sf){a,b,a,b}; }
v4sf fabcd(float a, float b, float c, float d) { return (v4sf){a,b,c,d}; }

v4sf fm000() { return (v4sf){m,0.0f,0.0f,0.0f}; }
v4sf fm0m0() { return (v4sf){m,0.0f,m,0.0f}; }
v4sf fmmmm() { return (v4sf){m,m,m,m}; }

/* { dg-final { scan-assembler-times "movlhps" 7 } } */
/* { dg-final { scan-assembler-times "movss" 20 } } */
/* { dg-final { scan-assembler-times "shufps" 7 } } */
/* { dg-final { scan-assembler-times "unpcklps" 5 } } */
/* { dg-final { scan-assembler-times "xorps" 3 } } */
