/* { dg-do compile } */
/* { dg-options "-fstrub=internal -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* g becomes STRUB_INTERNAL, because of the flag.  */
static void
g() {
}

/* f becomes STRUB_INTERNAL because of the flag, and gets split into
   STRUB_WRAPPER and STRUB_WRAPPED.  */
void
f() {
  g();
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 2 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 4 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapped\[)\]" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapper\[)\]" 2 "strub" } } */
