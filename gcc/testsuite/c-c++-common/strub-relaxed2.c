/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* The difference between relaxed and strict in this case is that we accept the
   call from one internal-strub function to another.  */

#include "strub-strict2.c"

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 2 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 4 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapped\[)\]" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapper\[)\]" 2 "strub" } } */
