/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -ftree-vectorize -mfpmath=sse -msse2 -mtune=atom" } */

#define STATIC static

#include "vect-double-2.c"
#include "sse2-check.h"
