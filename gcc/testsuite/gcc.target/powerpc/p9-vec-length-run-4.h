#include "p9-vec-length-4.h"

/* Check more to ensure vector access with out of bound.  */
#define N  144
/* Array size used for test function actually.  */
#define NF 123

#define run(TYPE)                                                              \
  {                                                                            \
    unsigned int i = 0;                                                        \
    TYPE a[N], b[N * 2], c[N * 4];                                             \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	a[i] = i + i % 2;                                                      \
	b[i * 2] = i * 2 + i % 3;                                              \
	b[i * 2 + 1] = i * 3 + i % 4;                                          \
	c[i * 4] = i * 4 + i % 5;                                              \
	c[i * 4 + 1] = i * 5 + i % 6;                                          \
	c[i * 4 + 2] = i * 6 + i % 7;                                          \
	c[i * 4 + 3] = i * 7 + i % 8;                                          \
      }                                                                        \
    test_mv_##TYPE (a, b, c, NF);                                              \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	TYPE a1 = i + i % 2;                                                   \
	TYPE b1 = i * 2 + i % 3;                                               \
	TYPE b2 = i * 3 + i % 4;                                               \
	TYPE c1 = i * 4 + i % 5;                                               \
	TYPE c2 = i * 5 + i % 6;                                               \
	TYPE c3 = i * 6 + i % 7;                                               \
	TYPE c4 = i * 7 + i % 8;                                               \
                                                                               \
	TYPE exp_a = a1;                                                       \
	TYPE exp_b1 = b1;                                                      \
	TYPE exp_b2 = b2;                                                      \
	TYPE exp_c1 = c1;                                                      \
	TYPE exp_c2 = c2;                                                      \
	TYPE exp_c3 = c3;                                                      \
	TYPE exp_c4 = c4;                                                      \
	if (i < NF)                                                            \
	  {                                                                    \
	    exp_a += 1;                                                        \
	    exp_b1 += 2;                                                       \
	    exp_b2 += 3;                                                       \
	    exp_c1 += 4;                                                       \
	    exp_c2 += 5;                                                       \
	    exp_c3 += 6;                                                       \
	    exp_c4 += 7;                                                       \
	  }                                                                    \
	if (a[i] != exp_a || b[i * 2] != exp_b1 || b[i * 2 + 1] != exp_b2      \
	    || c[i * 4] != exp_c1 || c[i * 4 + 1] != exp_c2                    \
	    || c[i * 4 + 2] != exp_c3 || c[i * 4 + 3] != exp_c4)               \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int
main (void)
{
  TEST_ALL (run)
  return 0;
}
