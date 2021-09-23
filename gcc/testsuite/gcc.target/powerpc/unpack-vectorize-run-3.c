/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "unpack-vectorize-3.h"

/* Test if unpack vectorization cases on signed int run successfully.  */

CHECK1 (si, sll)

int
main ()
{
  check1_si_sll ();
  return 0;
}
