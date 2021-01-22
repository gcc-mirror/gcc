/* { dg-options "-O2 -maltivec" } */

#include <stddef.h>
#include <altivec.h>
#include "pr79251.h"

TEST_VEC_INSERT_ALL (test)

#define run_test(TYPE, num)                                                    \
  {                                                                            \
    vector TYPE v;                                                             \
    vector TYPE u = {0x0};                                                     \
    for (long k = 0; k < 16 / sizeof (TYPE); k++)                              \
      v[k] = 0xaa;                                                             \
    for (long k = 0; k < 16 / sizeof (TYPE); k++)                              \
      {                                                                        \
	u = test##num (v, 254, k);                                             \
	if (u[k] != (TYPE) 254)                                                \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int
main (void)
{
  TEST_VEC_INSERT_ALL (run_test)
  return 0;
}
