/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_ADD" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_SUB" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MUL" "optimized" } } */
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
	a[i] = d[i] OP e[i];			\
      else					\
	a[i] = MAX(d[i], e[i]);			\
  }


BIN (add, +);
BIN (sub, -);
BIN (mul, *);
