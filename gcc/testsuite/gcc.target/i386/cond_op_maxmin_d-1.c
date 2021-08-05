/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_MAX" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MIN" "optimized" } } */
/* { dg-final { scan-assembler-times "vpmaxsd"  1 } } */
/* { dg-final { scan-assembler-times "vpminsd"  1 } } */

typedef char int8;
typedef unsigned char uint8;
typedef short int16;
typedef unsigned short uint16;
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
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) < (Y) ? (Y) : (X))

#define BIN(OPNAME, OP)				\
  void						\
  __attribute__ ((noipa,optimize ("O3")))	\
  foo_##OPNAME ()				\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	a[i] = OP(d[i], e[i]);			\
      else					\
	a[i] = d[i] - e[i];			\
  }

BIN (max, MAX);
BIN (min, MIN);
