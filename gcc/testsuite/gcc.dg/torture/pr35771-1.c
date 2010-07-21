/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target sse2_runtime } */

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

#define TYPE __m128

#include "pr35771.h"
