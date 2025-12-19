/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
#include <math.h>

/* This test is the original, platform-specific, version of
   gcc.dg/tree-ssa/sincos-ice-on-ifn_sin-call.c. */
void
foo (float output[1024 * 1024], const float input[1024 * 1024])
{
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    output[i] = __builtin_sinf (input[i]) + __builtin_cosf (input[i]);
  }
}
