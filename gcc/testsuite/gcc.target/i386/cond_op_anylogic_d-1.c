/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_AND" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_XOR" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_IOR" "optimized" } } */
/* { dg-final { scan-assembler-times "vpxord"  1 } } */
/* { dg-final { scan-assembler-times "vpord"  1 } } */
/* { dg-final { scan-assembler-times "vpandd"  1 } } */

typedef int int32;
typedef unsigned int uint32;
typedef long long int64;
typedef unsigned long long uint64;

#ifndef NUM
#define NUM 800
#endif
#ifndef TYPE
#define TYPE int
#endif

TYPE a[NUM], b[NUM], c[NUM], d[NUM], e[NUM], j[NUM];

#define BIN(OPNAME, OP)				\
  void						\
  __attribute__ ((noipa,optimize ("O3")))	\
  foo_##OPNAME ()				\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	a[i] = d[i] OP e[i];			\
      else					\
	a[i] = d[i] - e[i];			\
  }

BIN (and, &);
BIN (ior, |);
BIN (xor, ^);
