/* With -fwrapv the sums can wrap, so none of these are averages.  */
/* { dg-do compile } */
/* { dg-options "-O3 -fwrapv" } */

#include "vect_hadd_same_width_1.c"

/* { dg-final { scan-assembler-not {\t[su](r)?hadd\t} } } */
