/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_MAX" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MIN" "optimized" } } */
/* { dg-final { scan-assembler-times "vmaxpd"  1 } } */
/* { dg-final { scan-assembler-times "vminpd"  1 } } */

#include<math.h>
#ifndef NUM
#define NUM 800
#endif
#ifndef TYPE
#define TYPE double
#endif
#ifndef FN_MAX
#define FN_MAX fmax
#endif
#ifndef FN_MIN
#define FN_MIN fmin
#endif

TYPE a[NUM], b[NUM], c[NUM], d[NUM], e[NUM], j[NUM];
#define MAX FN_MAX
#define MIN FN_MIN

#define BIN(OPNAME, OP)				\
  void						\
  __attribute__ ((noipa,optimize ("Ofast")))	\
  foo_##OPNAME ()				\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	a[i] = (OP (d[i], e[i]));		\
      else					\
	a[i] = d[i] - e[i];			\
  }

BIN (max, MAX);
BIN (min, MIN);
