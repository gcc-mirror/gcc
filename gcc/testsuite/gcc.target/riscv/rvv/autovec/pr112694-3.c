/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvfh_zfh -mabi=lp64d -fdiagnostics-plain-output -flto -ffat-lto-objects -ftree-vectorize -fno-tree-loop-distribute-patterns -fno-vect-cost-model -fno-common -O3" } */

#define VECTOR_BITS 512
#define N (VECTOR_BITS * 11 / 64 + 4)

#define add(A, B) ((A) + (B))

#define DEF(OP)								\
  void __attribute__ ((noipa))						\
  f_##OP (double *restrict a, double *restrict b, double x)		\
  {									\
    for (int i = 0; i < N; i += 2)					\
      {									\
	a[i] = b[i] < 100 ? OP (b[i], x) : b[i];			\
	a[i + 1] = b[i + 1] < 70 ? OP (b[i + 1], x) : b[i + 1];		\
      }									\
  }

#define TEST(OP)						\
  {								\
    f_##OP (a, b, 10);						\
    _Pragma("GCC novector")					\
    for (int i = 0; i < N; ++i)					\
      {								\
	int bval = (i % 17) * 10;				\
	int truev = OP (bval, 10);				\
	if (a[i] != (bval < (i & 1 ? 70 : 100) ? truev : bval))	\
	__builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

#define FOR_EACH_OP(T)				\
  T (add)					\

FOR_EACH_OP (DEF)
