/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512dq -mno-avx512vl" } */

typedef int v16si __attribute__ ((vector_size (64)));
typedef float v16sf __attribute__ ((vector_size (64)));
typedef long long v8di __attribute__ ((vector_size (64)));
typedef double v8df __attribute__ ((vector_size (64)));

int f1 (v16si v) { return v[4]; }
float f2 (v16sf v) { return v[4]; }
long long f3 (v8di v) { return v[2]; }
double f4 (v8df v) { return v[2]; }

/* { dg-final { scan-assembler-not "vextract\[^\n\]*%ymm" } } */
/* { dg-final { scan-assembler-times "vextract\[if\]32x4\[\t \]+\\\$1, %zmm" 2 } } */
/* { dg-final { scan-assembler-times "vextract\[if\]64x2\[\t \]+\\\$1, %zmm" 2 } } */
