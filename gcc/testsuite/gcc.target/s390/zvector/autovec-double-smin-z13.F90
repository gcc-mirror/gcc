! { dg-do compile }
! { dg-options "-ffree-line-length-256 -O3 -march=z13 -mzvector -mzarch" }

#include "autovec-fortran.h"

AUTOVEC_FORTRAN (min)

! Fortran's min does not specify whether or not an exception should be raised in
! face of qNaNs, and neither does gcc's smin.  Vectorize min using quiet
! comparison, because that's the only one we have on z13.
! { dg-final { scan-assembler {\n\tvfchdb\t} } }
