/* { dg-do run { target { ia32 } } } */
/* { dg-options "-Os -march=i686 -mfpmath=sse -msse2" } */

#include<stdlib.h>
int
main (void)
{
  static volatile unsigned int ivin, ivout;
  static volatile _Float16 fv1, fv2;
  ivin = ((unsigned int)1);
  fv1 = ((unsigned int)1);
  fv2 = ivin;
  ivout = fv2;
  if (ivout != ((unsigned int)1))
    abort ();

  exit (0);
}
