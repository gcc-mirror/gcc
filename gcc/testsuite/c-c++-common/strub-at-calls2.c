/* { dg-do compile } */
/* { dg-options "-fstrub=at-calls -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* g does NOT become STRUB_AT_CALLS because it's not viable.  Without inline,
   force_output is set for static non-inline functions when not optimizing, and
   that keeps only_called_directly_p from returning true, which makes
   STRUB_AT_CALLS non-viable.  It becomes STRUB_CALLABLE instead.  */
static void
g() {
}

/* f does NOT become STRUB_AT_CALLS because it is visible; it becomes
   STRUB_CALLABLE.  */
void
f() {
  g();
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]callable\[)\]" 2 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]callable\[)\]" 2 "strub" } } */
