/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_ok } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#define N 64
#define TYPE unsigned

#include "vect-dot-qi.h"

/* { dg-final { scan-assembler-times {vudot\.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 4 } } */

