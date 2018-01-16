/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math" } */

#pragma GCC target "+nosve"

#include "vect-fmax-fmin.x"

/* { dg-final { scan-assembler "fmaxnm\\tv" } } */
/* { dg-final { scan-assembler "fminnm\\tv" } } */
