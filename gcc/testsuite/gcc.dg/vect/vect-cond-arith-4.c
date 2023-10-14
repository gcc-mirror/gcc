/* { dg-additional-options "-fdump-tree-optimized" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 11 / 64 + 3)

#define add(A, B) ((A) + (B))
#define sub(A, B) ((A) - (B))
#define mul(A, B) ((A) * (B))
#define div(A, B) ((A) / (B))

#define DEF(OP)							\
  void __attribute__ ((noipa))					\
  f_##OP (double *restrict a, double *restrict b, double x)	\
  {								\
    for (int i = 0; i < N; ++i)					\
      a[i] = b[i] < 100 ? OP (b[i], x) : b[i];			\
  }

#define TEST(OP)					\
  {							\
    f_##OP (a, b, 10);					\
    _Pragma("GCC novector")				\
    for (int i = 0; i < N; ++i)				\
      {							\
	int bval = (i % 17) * 10;			\
	int truev = OP (bval, 10);			\
	if (a[i] != (bval < 100 ? truev : bval))	\
	__builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

#define FOR_EACH_OP(T)				\
  T (add)					\
  T (sub)					\
  T (mul)					\
  T (div)

FOR_EACH_OP (DEF)

int
main (void)
{
  double a[N], b[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = (i % 17) * 10;
      asm volatile ("" ::: "memory");
    }
  FOR_EACH_OP (TEST)
  return 0;
}

/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?ADD} "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?SUB} "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?MUL} "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?RDIV} "optimized" { target vect_double_cond_arith } } } */
/* { dg-final { scan-tree-dump-not {VEC_COND_EXPR} "optimized" { target vect_double_cond_arith } } } */
