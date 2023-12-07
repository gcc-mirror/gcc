/* { dg-do compile } */
/* { dg-options "-O3 -fstrub=strict -fno-exceptions -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that the expected strub calls are issued.  At -O3 and -Os, we omit
   enter and leave calls within strub contexts, passing on the enclosing
   watermark.  */

#include "torture/strub-callable1.c"

/* { dg-final { scan-ipa-dump-times "strub_enter" 15 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_update" 4 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_leave" 15 "strub" } } */
