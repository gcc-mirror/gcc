/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mdouble-float -fno-math-errno -fno-fp-int-builtin-inexact -mlasx" } */

#include "vect-frint.c"

/* ceil */
/* { dg-final { scan-assembler "bl\t%plt\\(ceil\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(ceilf\\)" } } */
/* { dg-final { scan-assembler-not "\tvfrintrp\.s" } } */
/* { dg-final { scan-assembler-not "\tvfrintrp\.d" } } */
/* { dg-final { scan-assembler-not "\txvfrintrp\.s" } } */
/* { dg-final { scan-assembler-not "\txvfrintrp\.d" } } */

/* floor */
/* { dg-final { scan-assembler "bl\t%plt\\(floor\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(floorf\\)" } } */
/* { dg-final { scan-assembler-not "\tvfrintrm\.s" } } */
/* { dg-final { scan-assembler-not "\tvfrintrm\.d" } } */
/* { dg-final { scan-assembler-not "\txvfrintrm\.s" } } */
/* { dg-final { scan-assembler-not "\txvfrintrm\.d" } } */

/* nearbyint + rint: Only rint is allowed */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyint\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyintf\\)" } } */
/* { dg-final { scan-assembler-times "\tvfrint\.s" 1 } } */
/* { dg-final { scan-assembler-times "\tvfrint\.d" 1 } } */
/* { dg-final { scan-assembler-times "\txvfrint\.s" 1 } } */
/* { dg-final { scan-assembler-times "\txvfrint\.d" 1 } } */

/* round: we don't have a corresponding instruction */
/* { dg-final { scan-assembler "bl\t%plt\\(round\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundf\\)" } } */

/* roundeven */
/* { dg-final { scan-assembler "bl\t%plt\\(roundeven\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundevenf\\)" } } */
/* { dg-final { scan-assembler-not "\tvfrintrne\.s" } } */
/* { dg-final { scan-assembler-not "\tvfrintrne\.d" } } */
/* { dg-final { scan-assembler-not "\txvfrintrne\.s" } } */
/* { dg-final { scan-assembler-not "\txvfrintrne\.d" } } */

/* trunc */
/* { dg-final { scan-assembler "bl\t%plt\\(trunc\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(truncf\\)" } } */
/* { dg-final { scan-assembler-not "\tvfrintrz\.s" } } */
/* { dg-final { scan-assembler-not "\tvfrintrz\.d" } } */
/* { dg-final { scan-assembler-not "\txvfrintrz\.s" } } */
/* { dg-final { scan-assembler-not "\txvfrintrz\.d" } } */
