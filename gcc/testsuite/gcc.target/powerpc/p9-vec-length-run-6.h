#include "p9-vec-length-6.h"

/* Check more to ensure vector access with out of bound.  */
#define N 275
/* Array size used for test function actually.  */
#define NF 255

#define run(TYPE1, TYPE2)                                                      \
  {                                                                            \
    unsigned int i = 0;                                                        \
    TYPE1 a[N * 2];                                                            \
    TYPE2 b[N * 4];                                                            \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	a[i * 2] = i * 2 + i % 3;                                              \
	a[i * 2 + 1] = i * 3 + i % 4;                                          \
	b[i * 4] = i * 4 + i / 5;                                              \
	b[i * 4 + 1] = i * 5 + i / 6;                                          \
	b[i * 4 + 2] = i * 6 + i / 7;                                          \
	b[i * 4 + 3] = i * 7 + i / 8;                                          \
      }                                                                        \
    test_mv_##TYPE1##TYPE2 (a, b, NF);                                         \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	TYPE1 exp_a1 = i * 2 + i % 3;                                          \
	TYPE1 exp_a2 = i * 3 + i % 4;                                          \
	TYPE2 exp_b1 = i * 4 + i / 5;                                          \
	TYPE2 exp_b2 = i * 5 + i / 6;                                          \
	TYPE2 exp_b3 = i * 6 + i / 7;                                          \
	TYPE2 exp_b4 = i * 7 + i / 8;                                          \
	if (i < NF)                                                            \
	  {                                                                    \
	    exp_a1 += 1;                                                       \
	    exp_a2 += 2;                                                       \
	    exp_b1 += 3;                                                       \
	    exp_b2 += 4;                                                       \
	    exp_b3 += 5;                                                       \
	    exp_b4 += 6;                                                       \
	  }                                                                    \
	if (a[i * 2] != exp_a1 || a[i * 2 + 1] != exp_a2 || b[i * 4] != exp_b1 \
	    || b[i * 4 + 1] != exp_b2 || b[i * 4 + 2] != exp_b3                \
	    || b[i * 4 + 3] != exp_b4)                                         \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int
main (void)
{
  TEST_ALL2 (run)
  return 0;
}
