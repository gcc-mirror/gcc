/* { dg-do compile } */
/* { dg-options "-O0 -fstrub=strict -fno-exceptions -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that the expected strub calls are issued.  */

#include "torture/strub-callable1.c"

/* { dg-final { scan-ipa-dump-times "strub_enter" 45 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_update" 4 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_leave" 45 "strub" } } */
