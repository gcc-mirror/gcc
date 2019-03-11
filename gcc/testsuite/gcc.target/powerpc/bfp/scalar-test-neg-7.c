/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_neg (float *p)
{
  float source = *p;

  return scalar_test_neg (source);
}

int
main ()
{
  float neg_number = (float) -1;
  float plus_number = (float) 1;

  if (!test_neg (&neg_number))
    abort ();
  if (test_neg (&plus_number))
    abort ();
  return 0;
}
