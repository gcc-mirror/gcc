/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "unpack-vectorize-2.h"

/* Test if unpack vectorization cases on unsigned int run successfully.  */

CHECK1 (ui, ull)

int
main ()
{
  check1_ui_ull ();
  return 0;
}
