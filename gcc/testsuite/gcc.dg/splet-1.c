/* Test inl-sparc.h.  */

/* { dg-do run { target sparclet-*-* } } */
/* { dg-options -mcpu=sparclet } */

#include <inl-sparc.h>

main ()
{
  int a,b;

  a = scan (1, 2);
  if (a != 3)
    abort ();

  b = shuffle (4, 5);
  if (b != 6)
    abort ();

  exit (0);
}
