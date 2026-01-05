/* { dg-do compile } */
/* { dg-options "-O2 -moverride=sve_width=2048 -mtune=generic -ftree-vectorize" } */

#include "unpacked_cond_frinta_1.c"

/* Test that operations that can raise FE exceptions are properly masked
   even when not using -fno-trapping-math.  However when using masking we hit
   the existing problem where the vectorizer tries to match VF rather than
   number of elements.  This means we could have generated better code here
   using widening loads, but we can't as the vectorizer forces an unrolling.  */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 4 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 3 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tfrinta\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfrinta\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tsel\t} 6 { xfail *-*-* } } } */
