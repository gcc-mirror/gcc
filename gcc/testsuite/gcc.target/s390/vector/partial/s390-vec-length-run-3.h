#include "s390-vec-length-3.h"

#define decl(TYPE)                                                             \
  TYPE a_##TYPE[N_##TYPE];                                                     \
  TYPE b_##TYPE[N_##TYPE];                                                     \
  TYPE c_##TYPE[N_##TYPE];

#define run(TYPE)                                                              \
  {                                                                            \
    unsigned int i = 0;                                                        \
    for (i = 0; i < N_##TYPE; i++)                                             \
      {                                                                        \
	a_##TYPE[i] = i * 2 + 1;                                               \
	b_##TYPE[i] = i % 2 - 2;                                               \
      }                                                                        \
    test##TYPE ();                                                             \
    for (i = 0; i < N_##TYPE; i++)                                             \
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
