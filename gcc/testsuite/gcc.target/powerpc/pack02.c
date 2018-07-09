/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mhard-float" } */

#include <stddef.h>
#include <stdlib.h>
#include <math.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#if defined(__LONG_DOUBLE_IEEE128__)
/* If long double is IEEE 128-bit, we need to use the __ibm128 type instead of
   long double, and to use the appropriate pack/unpack routines.  We can't use
   __ibm128 on systems that don't support IEEE 128-bit floating point, because
   the type is not enabled on those systems.  */
#define PACK __builtin_pack_ibm128
#define UNPACK __builtin_unpack_ibm128
#define LDOUBLE __ibm128

#elif defined(__LONG_DOUBLE_IBM128__)
#define PACK __builtin_pack_longdouble
#define UNPACK __builtin_unpack_longdouble
#define LDOUBLE long double

#else
#error "long double must be either IBM 128-bit or IEEE 128-bit"
#endif

int
main (void)
{
  double high = pow (2.0, 60);
  double low  = 2.0;
  LDOUBLE a = ((LDOUBLE)high) + ((LDOUBLE)low);
  double x0 = UNPACK (a, 0);
  double x1 = UNPACK (a, 1);
  LDOUBLE b = PACK (x0, x1);

#ifdef DEBUG
  {
    size_t i;
    union {
      LDOUBLE ld;
      double d;
      unsigned char uc[sizeof (LDOUBLE)];
      char c[sizeof (LDOUBLE)];
    } u;

    printf ("a  = 0x");
    u.ld = a;
    for (i = 0; i < sizeof (LDOUBLE); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", a);

    printf ("b  = 0x");
    u.ld = b;
    for (i = 0; i < sizeof (LDOUBLE); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", b);

    printf ("hi = 0x");
    u.d = high;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (LDOUBLE) - sizeof (double))), "", high);

    printf ("lo = 0x");
    u.d = low;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (LDOUBLE) - sizeof (double))), "", low);

    printf ("x0 = 0x");
    u.d = x0;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (LDOUBLE) - sizeof (double))), "", x0);

    printf ("x1 = 0x");
    u.d = x1;
    for (i = 0; i < sizeof (double); i++)
      printf ("%.2x", u.uc[i]);

    printf (",%*s %g\n", (int)(2 * (sizeof (LDOUBLE) - sizeof (double))), "", x1);
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
