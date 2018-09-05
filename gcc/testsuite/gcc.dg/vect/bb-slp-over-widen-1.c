/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

/* Deliberate use of signed >>.  */
#define DEF_LOOP(SIGNEDNESS)			\
  void __attribute__ ((noipa))			\
  f_##SIGNEDNESS (SIGNEDNESS char *restrict a,	\
		  SIGNEDNESS char *restrict b,	\
		  SIGNEDNESS char *restrict c)	\
  {						\
    a[0] = (b[0] + c[0]) >> 1;			\
    a[1] = (b[1] + c[1]) >> 1;			\
    a[2] = (b[2] + c[2]) >> 1;			\
    a[3] = (b[3] + c[3]) >> 1;			\
    a[4] = (b[4] + c[4]) >> 1;			\
    a[5] = (b[5] + c[5]) >> 1;			\
    a[6] = (b[6] + c[6]) >> 1;			\
    a[7] = (b[7] + c[7]) >> 1;			\
    a[8] = (b[8] + c[8]) >> 1;			\
    a[9] = (b[9] + c[9]) >> 1;			\
    a[10] = (b[10] + c[10]) >> 1;		\
    a[11] = (b[11] + c[11]) >> 1;		\
    a[12] = (b[12] + c[12]) >> 1;		\
    a[13] = (b[13] + c[13]) >> 1;		\
    a[14] = (b[14] + c[14]) >> 1;		\
    a[15] = (b[15] + c[15]) >> 1;		\
  }

DEF_LOOP (signed)
DEF_LOOP (unsigned)

#define N 16

#define TEST_LOOP(SIGNEDNESS, BASE_B, BASE_C)		\
  {							\
    SIGNEDNESS char a[N], b[N], c[N];			\
    for (int i = 0; i < N; ++i)				\
      {							\
	b[i] = BASE_B + i * 15;				\
	c[i] = BASE_C + i * 14;				\
	asm volatile ("" ::: "memory");			\
      }							\
    f_##SIGNEDNESS (a, b, c);				\
    for (int i = 0; i < N; ++i)				\
      if (a[i] != (BASE_B + BASE_C + i * 29) >> 1)	\
	__builtin_abort ();				\
  }

int
main (void)
{
  check_vect ();

  TEST_LOOP (signed, -128, -120);
  TEST_LOOP (unsigned, 4, 10);

  return 0;
}

/* { dg-final { scan-tree-dump "demoting int to signed short" "slp2" { target { ! vect_widen_shift } } } } */
/* { dg-final { scan-tree-dump "demoting int to unsigned short" "slp2" { target { ! vect_widen_shift } } } } */
/* { dg-final { scan-tree-dump {\.AVG_FLOOR} "slp2" { target vect_avg_qi } } } */
/* { dg-final { scan-tree-dump-times "basic block vectorized" 2 "slp2" } } */
