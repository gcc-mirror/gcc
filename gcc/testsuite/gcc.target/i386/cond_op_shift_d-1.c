/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times ".COND_SHR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times ".COND_SHL" 2 "optimized" } } */
/* { dg-final { scan-assembler-times "vpsrad"  1 } } */
/* { dg-final { scan-assembler-times "vpslld"  1 } } */
/* { dg-final { scan-assembler-times "vpsravd"  1 } } */
/* { dg-final { scan-assembler-times "vpsllvd"  1 } } */


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

#define BINC(OPNAME, OP)			\
  void						\
  __attribute__ ((noipa,optimize ("O3")))	\
  foo_##OPNAME##_const ()			\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	a[i] = d[i] OP 3;			\
      else					\
	a[i] = MAX(d[i], e[i]);		\
  }

#define BINV(OPNAME, OP)			\
  void						\
  __attribute__ ((noipa,optimize ("O3")))	\
  foo_##OPNAME##_variable ()			\
  {						\
    for (int i = 0; i != NUM; i++)		\
      if (b[i] < c[i])				\
	a[i] = d[i] OP e[i];			\
      else					\
	a[i] = MAX(d[i], e[i]);		\
  }

BINC (shl, <<);
BINC (shr, >>);
BINV (shl, <<);
BINV (shr, >>);
