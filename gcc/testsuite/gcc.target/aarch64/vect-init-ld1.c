/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef char v8qi __attribute__ ((vector_size (8)));
typedef char v16qi __attribute__ ((vector_size (16)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__ ((vector_size (16)));

typedef __fp16 v4hf __attribute__ ((vector_size (8)));
typedef __fp16 v8hf __attribute__ ((vector_size (16)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

#define FUNC2(T, IT)    \
T                       \
foo_##T (IT *a, IT *b)  \
{                       \
  T res = { *a, *b };   \
  return res;           \
}

FUNC2(v2di, long long)
FUNC2(v2si, int)
FUNC2(v2df, double)
FUNC2(v2sf, float)

#define FUNC4(T, IT)    \
T                       \
foo_##T (IT *a, IT *b, IT *c, IT *d)    \
{                                       \
  T res = { *a, *b, *c, *d };           \
  return res;                           \
}

FUNC4(v4si, int)
FUNC4(v4hi, short)
FUNC4(v4sf, float)
FUNC4(v4hf, __fp16)

#define FUNC8(T, IT)    \
T                       \
foo_##T (IT *a, IT *b, IT *c, IT *d, IT *e, IT *f, IT *g, IT *h)        \
{                                                                       \
  T res = { *a, *b, *c, *d, *e, *f, *g, *h };                           \
  return res;                                                           \
}

FUNC8(v8hi, short)
FUNC8(v8qi, char)
FUNC8(v8hf, __fp16)


v16qi
foo_v16qi (char *a, char *a1, char *a2, char *a3, char *a4, char *a5,
     char *a6, char *a7, char *a8, char *a9, char *a10, char *a11, char *a12,
     char *a13, char *a14, char *a15)
{
  v16qi res = { *a, *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8, *a9, *a10,
               *a11, *a12, *a13, *a14, *a15 };
  return res;
}

/* { dg-final { scan-assembler-not "ld1r\t" } } */
/* { dg-final { scan-assembler-not "dup\t" } } */
/* { dg-final { scan-assembler-not "ins\t" } } */
