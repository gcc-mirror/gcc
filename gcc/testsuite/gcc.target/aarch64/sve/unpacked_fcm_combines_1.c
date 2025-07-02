/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 --param=aarch64-autovec-preference=sve-only -fno-trapping-math" } */

#include "unpacked_fcm_1.c"

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 32 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 32 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 32 } } */

/* Drop a PTRUE predicated AND with the loop mask and comparison result in
   favour of predicating the comparison with the loop mask.  */
/* { dg-final { scan-assembler-not {\tand\t} } } */

/* Similarly, for codes that are implemented via an inversion, prefer
   NOT (predicated with the loop mask) over BIC+PTRUE.  */
/* { dg-final { scan-assembler-not {\tbic\t} } } */
/* { dg-final { scan-assembler-times {\tnot\t} 15 } } */
