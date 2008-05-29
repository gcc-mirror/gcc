/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */

typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

#define TYPE __m128d

#include "pr35771.h"
