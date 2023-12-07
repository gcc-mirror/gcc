/* { dg-do compile } */
/* { dg-options "-fstrub=at-calls -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* h becomes STRUB_CALLABLE, rather than STRUB_INLINABLE, because of the
   strub-enabling -fstrub flag, and gets inlined before pass_ipa_strub.  */
static inline void
__attribute__ ((__always_inline__))
h() {
}

/* g becomes STRUB_AT_CALLS, because of the flag.  */
static inline void
g() {
  h();
}

/* f does NOT become STRUB_AT_CALLS because it is visible; it becomes
   STRUB_CALLABLE.  */
void
f() {
  g();
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 3 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]at-calls\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]callable\[)\]" 2 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]at-calls\[)\]" 1 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]callable\[)\]" 1 "strub" } } */
