/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-options "-O2 -mhard-float" } */

#include <stddef.h>
#include <stdlib.h>
#include <math.h>

#ifdef DEBUG
#include <stdio.h>
#endif

int
main (void)
{
  double high = pow (2.0, 60);
  double low  = 2.0;
  long double a = ((long double)high) + ((long double)low);
  double x0 = __builtin_unpack_longdouble (a, 0);
  double x1 = __builtin_unpack_longdouble (a, 1);
  long double b = __builtin_pack_longdouble (x0, x1);

#ifdef DEBUG
  {
    size_t i;
    union {
      long double ld;
      double d;
      unsigned char uc[sizeof (long double)];
      char c[sizeof (long double)];
    } u;

    printf ("a  = 0x");
    u.ld = a;
    for (i = 0; i < sizeof (long double); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", a);

    printf ("b  = 0x");
    u.ld = b;
    for (i = 0; i < sizeof (long double); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", b);

    printf ("hi = 0x");
    u.d = high;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (long double) - sizeof (double))), "", high);

    printf ("lo = 0x");
    u.d = low;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (long double) - sizeof (double))), "", low);

    printf ("x0 = 0x");
    u.d = x0;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (long double) - sizeof (double))), "", x0);

    printf ("x1 = 0x");
    u.d = x1;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (long double) - sizeof (double))), "", x1);
  }
#endif

  if (high != x0)
    abort ();

  if (low != x1)
    abort ();

  if (a != b)
    abort ();

  if (x0 != high)
    abort ();

  if (x1 != low)
    abort ();

  return 0;
}
