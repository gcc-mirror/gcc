/* { dg-do run { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mcpu=power8 -O2" } */

/* Check whether we get the right bits for direct move at runtime.  */

#define TYPE double
#define IS_FLOAT 1
#define NO_ALTIVEC 1
#define DO_MAIN

#include "direct-move.h"
