/* { dg-do compile } */
/* { dg-options "-fstrub=all -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* g becomes STRUB_INTERNAL, because of the flag.  Without inline, force_output
   is set for static non-inline functions when not optimizing, and that keeps
   only_called_directly_p from returning true, which makes STRUB_AT_CALLS
   non-viable.  */
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
