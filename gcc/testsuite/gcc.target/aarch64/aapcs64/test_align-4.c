/* Test AAPCS64 layout.

   C.3 If the argument is an HFA then the NSRN is set to 8 and the size
   of the argument is rounded up to the nearest multiple of 8 bytes.

   TODO: add the check of an HFA containing half-precision floating-point
   when __f16 is supported in A64 GCC.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-4.c"

struct z1
{
  double x[4];
};

struct z1 a = { 5.0, 6.0, 7.0, 8.0 };

struct z2
{
  float x[3];
};

struct z2 b = { 13.f, 14.f, 15.f };
struct z2 c = { 16.f, 17.f, 18.f };

#include "abitest.h"
#else

  ARG(struct z1, a, D0)
  ARG(double, 9.0, D4)
  ARG(double, 10.0, D5)
  ARG(struct z2, b, STACK)       /* [C.3] on stack and size padded to 16 bytes */
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(float, 15.5f, STACK+16)    /* [C.3] NSRN has been set to 8 */
#else
  ARG(float, 15.5f, STACK+20)
#endif
  LAST_ARG(struct z2, c, STACK+24)
#endif
