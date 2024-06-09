/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* The difference between relaxed and strict in this case is that we accept the
   call from one internal-strub function to another.  Without the error,
   inlining takes place.  */

#include "strub-strict1.c"

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 3 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]inlinable\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]at-calls-opt\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 1 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 3 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]at-calls-opt\[)\]" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapped\[)\]" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapper\[)\]" 1 "strub" } } */
