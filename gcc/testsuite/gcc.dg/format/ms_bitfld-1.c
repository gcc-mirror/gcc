/* Test for printf formats and bit-fields: bug 22421.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat" } */
/* { dg-require-effective-target int32plus } */

#define USE_SYSTEM_FORMATS
#include "format.h"

struct s {
  unsigned int u1 : 1;
  signed int s1 : 1;
  unsigned int u15 : 15;
  signed int s15 : 15;
  unsigned int u16 : 16;
  signed int s16 : 16;
  unsigned long u31 : 31;
  signed long s31 : 31;
  unsigned long u32 : 32;
  signed long s32 : 32;
  unsigned long long u48 : 48;
} x;

void
foo (void)
{
  printf ("%d%u", x.u1, x.u1);
  printf ("%d%u", x.s1, x.s1);
  printf ("%d%u", x.u15, x.u15);
  printf ("%d%u", x.s15, x.s15);
  printf ("%d%u", x.u16, x.u16);
  printf ("%d%u", x.s16, x.s16);
#if __INT_MAX__ > 32767
  /* If integers are 16 bits, there doesn't seem to be a way of
     printing these without getting an error.  */
  printf ("%d%u", x.u31, x.u31);
  printf ("%d%u", x.s31, x.s31);
#endif
#if __LONG_MAX__ > 2147483647 && __INT_MAX__ >= 2147483647
  /* If long is wider than 32 bits, the 32-bit bit-fields are int or
     unsigned int or promote to those types.  Otherwise, long is 32
     bits and the bit-fields are of type plain long or unsigned
     long.  */
  printf ("%d%u", x.u32, x.u32);
  printf ("%d%u", x.s32, x.s32);
#else
  printf ("%ld%lu", x.u32, x.u32);
  printf ("%ld%lu", x.s32, x.s32);
#endif
  printf ("%I64u", x.u48); /* { dg-warning "has type '.*unsigned int:48'" } */
  printf ("%I64u", (unsigned long long)x.u48);
}
