/* { dg-do run  { target { ! ia32 } } } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-require-effective-target sse4 } */

#include "sse4_1-check.h"

typedef unsigned short __attribute__((__vector_size__ (8))) U4;
typedef unsigned short __attribute__((__vector_size__ (4))) U2;

U4
__attribute__((noipa))
foo4 (U4 a, U4 b)
{
  return a > b;
}

U2
__attribute__((noipa))
foo2 (U2 a, U2 b)
{
  return a > b;
}

static void
sse4_1_test ()
{
  U4 a = __extension__(U4) {1, 1, 1, 1};
  U4 b = foo4 (a, a);
  if (b[0] || b[1] || b[2] || b[3]) __builtin_abort();

  U2 c = __extension__(U2) {1, 1};
  U2 d = foo2 (c, c);
  if (d[0] || d[1]) __builtin_abort();

  return;
}
