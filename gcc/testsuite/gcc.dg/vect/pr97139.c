/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int pix[4];

int __attribute__((noipa)) foo (void)
{
  pix[0] = pix[0] / 4;
  pix[1] = pix[1] / 4;
  pix[2] = pix[2] / 4;
  pix[3] = pix[3] / 4;
  return pix[0] + pix[1] + pix[2] + pix[3];
}

int main ()
{
  check_vect ();

  pix[0] = 8;
  pix[1] = 16;
  pix[2] = 32;
  pix[3] = 64;
  if (foo () != 30)
    __builtin_abort ();
  return 0;
}
