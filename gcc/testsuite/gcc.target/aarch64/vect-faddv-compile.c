
/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math" } */

#include "vect-faddv.x"

/* { dg-final { scan-assembler-times "faddp\\tv" 2} } */
