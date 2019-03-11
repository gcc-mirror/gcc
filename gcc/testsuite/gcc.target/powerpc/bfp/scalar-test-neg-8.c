/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_neg (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_test_neg (source);
}

int
main ()
{
  __ieee128 neg_number = (__ieee128) -1;
  __ieee128 plus_number = (__ieee128) 1;

  if (!test_neg (&neg_number))
    abort ();
  if (test_neg (&plus_number))
    abort ();
  return 0;
}

