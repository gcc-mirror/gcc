/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-additional-options "-O3 -march=armv8.2-a+dotprod" } */

#define N 64
#define TYPE signed

#include "vect-dot-qi.h"

/* { dg-final { scan-assembler-times {sdot\tv[0-9]+\.4s, v[0-9]+\.16b, v[0-9]+\.16b} 4 } } */
