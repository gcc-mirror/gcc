/* { dg-do run { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

#include <stddef.h>
#include <stdlib.h>
#include <altivec.h>
#include <string.h>

#ifdef DEBUG
#include <stdio.h>
#endif

typedef __int128_t __attribute__((__vector_size__(16)))	vector_128_t;
typedef __int128_t					scalar_128_t;
typedef	unsigned long long				scalar_64_t;

volatile scalar_64_t one = 1;
volatile scalar_64_t two = 2;

int
main (void)
{
  scalar_128_t a = (((scalar_128_t)one) << 64) | ((scalar_128_t)two);
  vector_128_t v1 = (vector_128_t) { a };
  vector_128_t v2 = __builtin_pack_vector_int128 (one, two);
  scalar_64_t x0 = __builtin_unpack_vector_int128 (v1, 0);
  scalar_64_t x1 = __builtin_unpack_vector_int128 (v1, 1);
  vector_128_t v3 = __builtin_pack_vector_int128 (x0, x1);

  size_t i;
  union {
    scalar_128_t i128;
    vector_128_t v128;
    scalar_64_t u64;
    unsigned char uc[sizeof (scalar_128_t)];
    char c[sizeof (scalar_128_t)];
  } u, u2;

#ifdef DEBUG
  {
    printf ("a  = 0x");
    u.i128 = a;
    for (i = 0; i < sizeof (scalar_128_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nv1 = 0x");
    u.v128 = v1;
    for (i = 0; i < sizeof (scalar_128_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nv2 = 0x");
    u.v128 = v2;
    for (i = 0; i < sizeof (scalar_128_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nv3 = 0x");
    u.v128 = v3;
    for (i = 0; i < sizeof (scalar_128_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nx0 = 0x");
    u.u64 = x0;
    for (i = 0; i < sizeof (scalar_64_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\nx1 = 0x");
    u.u64 = x1;
    for (i = 0; i < sizeof (scalar_64_t); i++)
      printf ("%.2x", u.uc[i]);

    printf ("\n");
  }
#endif

  u2.i128 = a;
  u.v128 = v1;
  if (memcmp (u.c, u2.c, sizeof (scalar_128_t)) != 0)
    abort ();

  u.v128 = v2;
  if (memcmp (u.c, u2.c, sizeof (scalar_128_t)) != 0)
    abort ();

  u.v128 = v3;
  if (memcmp (u.c, u2.c, sizeof (scalar_128_t)) != 0)
    abort ();

  return 0;
}
