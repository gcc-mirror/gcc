/* { dg-do compile } */
/* { dg-options "-O1 -fstrub=strict -fno-exceptions -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

#include "strub-tail-O2.c"

/* { dg-final { scan-ipa-dump-times "strub_enter" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_update" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_leave" 2 "strub" } } */
