/* Test AAPCS64 layout for HFA with zero-sized bit-field.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_28.c"

/* Anonymous bitfields do not add members; if they do not change the layout
   then the end result may still be an HFA.  */
struct z
{
  float a;
  int :0;
  float b;
};

struct z a = { 5.0f, 6.0f };
struct z b = { 9.0f, 10.0f };

#define MYFUNCTYPE struct z

#include "abitest.h"
#else
  ARG(int, 7, W0)
  ARG(struct z, a, S0)
  LAST_ARG(struct z, b, S2)
#endif
