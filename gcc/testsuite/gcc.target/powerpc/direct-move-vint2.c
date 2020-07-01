/* { dg-do run } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* Check whether we get the right bits for direct move at runtime.  */

#define TYPE vector int
#define DO_MAIN

#include "direct-move.h"
