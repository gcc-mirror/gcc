/* PR target/128221 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512" } */

#define N 1024

double a[N], b[N], c[N], d[N];
int m[N];

#define DEF_COND_FMA(NAME, EXPR, ELSE)		\
  void						\
  NAME (void)					\
  {						\
    for (int i = 0; i < N; i++)			\
      a[i] = m[i] ? (EXPR) : (ELSE);		\
  }

#define DEF_COND_FMA_ALL(NAME, EXPR)		\
  DEF_COND_FMA (NAME##_z, EXPR, 0.0)		\
  DEF_COND_FMA (NAME##_mul1, EXPR, b[i])	\
  DEF_COND_FMA (NAME##_mul2, EXPR, c[i])	\
  DEF_COND_FMA (NAME##_add, EXPR, d[i])

DEF_COND_FMA_ALL (fma, b[i] * c[i] + d[i])
DEF_COND_FMA_ALL (fms, b[i] * c[i] - d[i])
DEF_COND_FMA_ALL (fnma, d[i] - b[i] * c[i])
DEF_COND_FMA_ALL (fnms, -b[i] * c[i] - d[i])

/* { dg-final { scan-assembler "vfmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmadd\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler "vfnmsub\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vfn?m(?:add|sub)\[0-9\]+pd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vblendm" } } */
/* { dg-final { scan-assembler-not "vmov\[a-z0-9\]+\[ \\t\]+%zmm\[0-9\]+, %zmm\[0-9\]+\{%k" } } */
