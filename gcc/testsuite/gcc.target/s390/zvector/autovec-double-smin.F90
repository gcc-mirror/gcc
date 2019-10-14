! { dg-do compile }
! { dg-options "-ffree-line-length-256 -O3 -march=z14 -mzvector -mzarch" }

#include "autovec-fortran.h"

AUTOVEC_FORTRAN (min)

! { dg-final { scan-assembler {\n\tvfmindb\t} } }
