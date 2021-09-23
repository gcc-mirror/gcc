/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-vect" } */
/* { dg-final { scan-tree-dump ".COND_ADD" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_SUB" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_MUL" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_RDIV" "vect" } } */

#ifndef NUM
#define NUM 800
#endif
#ifndef TYPE
#define TYPE double
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
  }


BIN (add, +);
BIN (sub, -);
BIN (mul, *);
BIN (div, /);
