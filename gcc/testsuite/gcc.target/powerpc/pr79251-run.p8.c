/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -mdejagnu-cpu=power8" } */

#include <stddef.h>
#include <altivec.h>
#include "pr79251.h"

int
main (void)
{
  TEST_VEC_INSERT_ALL (run_test)
  return 0;
}
