/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "valign\[dq\]" 8 } } */
/* { dg-final { scan-assembler-times "vextract" 12 } } */

typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));
typedef double v4df __attribute__((vector_size(32)));
typedef double v8df __attribute__((vector_size(64)));
typedef long long v4di __attribute__((vector_size(32)));
typedef long long v8di __attribute__((vector_size(64)));

#define EXTRACT(V,S,IDX)			\
  S						\
  __attribute__((noipa))			\
  foo_##V##_##IDX (V v)				\
  {						\
    return v[IDX];				\
  }						\

EXTRACT (v8sf, float, 4);
EXTRACT (v8sf, float, 7);
EXTRACT (v8si, int, 4);
EXTRACT (v8si, int, 7);
EXTRACT (v16sf, float, 4);
EXTRACT (v16sf, float, 8);
EXTRACT (v16sf, float, 12);
EXTRACT (v16sf, float, 15);
EXTRACT (v16si, int, 4);
EXTRACT (v16si, int, 8);
EXTRACT (v16si, int, 12);
EXTRACT (v16si, int, 15);
EXTRACT (v4df, double, 2);
EXTRACT (v4df, double, 3);
EXTRACT (v4di, long long, 2);
EXTRACT (v4di, long long, 3);
EXTRACT (v8df, double, 4);
EXTRACT (v8df, double, 7);
EXTRACT (v8di, long long, 4);
EXTRACT (v8di, long long, 7);
