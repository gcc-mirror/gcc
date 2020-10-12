/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -ffast-math" } */

/* Fast-math implies -fno-trapping-math -fno-signaling-nans which imply
   that no user visible trap will happen.  */

#include "load-and-test-fp.h"

/* { dg-final { scan-assembler-times "ltdbr\t" 12 } } */
