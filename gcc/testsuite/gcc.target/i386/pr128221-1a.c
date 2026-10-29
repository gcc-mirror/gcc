/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512" } */

#define N 1024

double a[N], b[N], c[N];
int m[N];

#define DEF_COND_FMA(NAME, EXPR)		\
  void						\
  NAME (double w, double e)			\
  {						\
    for (int i = 0; i < N; i++)			\
      a[i] = m[i] ? (EXPR) : e;			\
  }

DEF_COND_FMA (fma_test, a[i] + b[i] * w)
DEF_COND_FMA (fms_test, b[i] * w - c[i])
DEF_COND_FMA (fnma_test, c[i] - b[i] * w)
DEF_COND_FMA (fnms_test, -b[i] * w - c[i])

/* { dg-final { scan-assembler "vfmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vfn?m(?:add|sub)\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" } } */
