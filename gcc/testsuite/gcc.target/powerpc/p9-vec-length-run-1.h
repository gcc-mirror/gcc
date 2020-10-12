#include "p9-vec-length-1.h"

#define decl(TYPE)                                                             \
  TYPE a_##TYPE[N];                                                            \
  TYPE b_##TYPE[N];                                                            \
  TYPE c_##TYPE[N];

#define run(TYPE)                                                              \
  {                                                                            \
    unsigned int i = 0;                                                        \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	a_##TYPE[i] = i * 2 + 1;                                               \
	b_##TYPE[i] = i % 2 - 2;                                               \
      }                                                                        \
    test##TYPE ();                                                             \
    for (i = 0; i < N; i++)                                                    \
      {                                                                        \
	TYPE a1 = i * 2 + 1;                                                   \
	TYPE b1 = i % 2 - 2;                                                   \
	TYPE exp_c = a1 + b1;                                                  \
	if (c_##TYPE[i] != exp_c)                                              \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

TEST_ALL (decl)

int
main (void)
{
  TEST_ALL (run)
  return 0;
}
