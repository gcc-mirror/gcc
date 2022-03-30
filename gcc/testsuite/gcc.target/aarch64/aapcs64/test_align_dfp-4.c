/* Test AAPCS64 layout.

   C.3 If the argument is an HFA then the NSRN is set to 8 and the size
   of the argument is rounded up to the nearest multiple of 8 bytes.

   TODO: add the check of an HFA containing half-precision floating-point
   when __f16 is supported in A64 GCC.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align_dfp-4.c"

struct z1
{
  _Decimal64 x[4];
};

struct z1 a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };

struct z2
{
  _Decimal32 x[3];
};

struct z2 b = { 13.df, 14.df, 15.df };
struct z2 c = { 16.df, 17.df, 18.df };

#include "abitest.h"
#else

  ARG(struct z1, a, D0)
  ARG(_Decimal64, 9.0dd, D4)
  ARG(_Decimal64, 10.0dd, D5)
  ARG(struct z2, b, STACK)       /* [C.3] on stack and size padded to 16 bytes */
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(_Decimal32, 15.5df, STACK+16)    /* [C.3] NSRN has been set to 8 */
#else
  ARG(_Decimal32, 15.5df, STACK+20)
#endif
  LAST_ARG(struct z2, c, STACK+24)
#endif
