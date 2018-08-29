/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math" } */

#pragma GCC target "+nosve"

#include "vect-faddv.x"

/* { dg-final { scan-assembler-times "faddp\\tv" 2} } */
