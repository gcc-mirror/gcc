/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mdouble-float -fno-math-errno -fno-fp-int-builtin-inexact -mlasx" } */

#include "vect-ftint.c"

/* ceil */
/* { dg-final { scan-assembler "bl\t%plt\\(ceil\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(ceilf\\)" } } */
/* { dg-final { scan-assembler-not "\tvftintrp\.w\.s" } } */
/* { dg-final { scan-assembler-not "\tvftintrp\.l\.d" } } */
/* { dg-final { scan-assembler-not "\txvftintrp\.w\.s" } } */
/* { dg-final { scan-assembler-not "\txvftintrp\.l\.d" } } */

/* floor */
/* { dg-final { scan-assembler "bl\t%plt\\(floor\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(floorf\\)" } } */
/* { dg-final { scan-assembler-not "\tvftintrm\.w\.s" } } */
/* { dg-final { scan-assembler-not "\tvftintrm\.l\.d" } } */
/* { dg-final { scan-assembler-not "\txvftintrm\.w\.s" } } */
/* { dg-final { scan-assembler-not "\txvftintrm\.l\.d" } } */

/* nearbyint + rint */
/* { dg-final { scan-assembler "bl\t%plt\\(floor\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(floorf\\)" } } */
/* { dg-final { scan-assembler-times "\tvftint\.w\.s" 1 } } */
/* { dg-final { scan-assembler-times "\tvftint\.l\.d" 1 } } */
/* { dg-final { scan-assembler-times "\txvftint\.w\.s" 1 } } */
/* { dg-final { scan-assembler-times "\txvftint\.l\.d" 1 } } */

/* round: we don't have a corresponding instruction */
/* { dg-final { scan-assembler "bl\t%plt\\(lround\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundf\\)" } } */

/* roundeven */
/* { dg-final { scan-assembler "bl\t%plt\\(roundeven\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundevenf\\)" } } */
/* { dg-final { scan-assembler-not "\tvftintrne\.w\.s" } } */
/* { dg-final { scan-assembler-not "\tvftintrne\.l\.d" } } */
/* { dg-final { scan-assembler-not "\txvftintrne\.w\.s" } } */
/* { dg-final { scan-assembler-not "\txvftintrne\.l\.d" } } */

/* { dg-final { scan-assembler "bl\t%plt\\(trunc\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(truncf\\)" } } */
