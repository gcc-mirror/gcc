/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "unpack-vectorize-1.h"

/* Test if unpack vectorization cases on signed/unsigned short and char
   run successfully.  */

CHECK1 (sh, si)
CHECK1 (uh, ui)
CHECK1 (sc, sh)
CHECK1 (uc, uh)

int
main ()
{
  check1_sh_si ();
  check1_uh_ui ();
  check1_sc_sh ();
  check1_uc_uh ();

  return 0;
}
