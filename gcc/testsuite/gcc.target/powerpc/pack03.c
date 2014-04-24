/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2" } */

#include <stddef.h>
#include <stdlib.h>
#include <math.h>

#ifdef DEBUG
#include <stdio.h>
#endif

int
main (void)
{
  _Decimal128 one	= (_Decimal128)1.0;
  _Decimal128 two	= (_Decimal128)2.0;
  _Decimal128 ten	= (_Decimal128)10.0;
  _Decimal128 a		= one;
  _Decimal128 b;
  _Decimal128 c;
  unsigned long long x0;
  unsigned long long x1;
  size_t i;

  for (i = 0; i < 25; i++)
    a *= ten;

  a += two;

  x0 = __builtin_unpack_dec128 (a, 0);
  x1 = __builtin_unpack_dec128 (a, 1);
  b = __builtin_pack_dec128 (x0, x1);
  c = __builtin_dscliq (one, 25) + two;

#ifdef DEBUG
  {
    union {
      _Decimal128 d;
      unsigned long long ull;
      unsigned char uc[sizeof (_Decimal128)];
    } u;

    printf ("a  = 0x");
    u.d = a;
    for (i = 0; i < sizeof (_Decimal128); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", (long double)a);

    printf ("b  = 0x");
    u.d = b;
    for (i = 0; i < sizeof (_Decimal128); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", (long double)b);

    printf ("c  = 0x");
    u.d = c;
    for (i = 0; i < sizeof (_Decimal128); i++)
      printf ("%.2x", u.uc[i]);

    printf (", %Lg\n", (long double)c);

    printf ("x0 = 0x");
    u.ull = x0;
    for (i = 0; i < sizeof (unsigned long long); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nx1 = 0x");
    u.ull = x1;
    for (i = 0; i < sizeof (unsigned long long); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\n");
  }
#endif

  if (a != b)
    abort ();

  if (a != c)
    abort ();

  return 0;
}
