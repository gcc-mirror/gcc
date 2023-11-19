/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-fp-int-builtin-inexact" } */

#include "vect-frint-scalar.c"

/* cannot use LSX for these with -fno-fp-int-builtin-inexact,
   call library function.  */
/* { dg-final { scan-assembler "\tb\t%plt\\(ceil\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(ceilf\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(floor\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(floorf\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(trunc\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(truncf\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(roundeven\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(roundevenf\\)" } } */

/* nearbyint is not allowed to rasie FE_INEXACT for decades */
/* { dg-final { scan-assembler "\tb\t%plt\\(nearbyint\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(nearbyintf\\)" } } */

/* rint should just use basic FP operation */
/* { dg-final { scan-assembler "\tfrint\.s" } } */
/* { dg-final { scan-assembler "\tfrint\.d" } } */
