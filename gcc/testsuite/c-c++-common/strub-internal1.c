/* { dg-do compile } */
/* { dg-options "-fstrub=internal -fdump-ipa-strubm -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* h becomes STRUB_CALLABLE, rather than STRUB_INLINABLE, because of the
   strub-enabling -fstrub flag, and gets inlined before pass_ipa_strub.  */
static inline void
__attribute__ ((__always_inline__))
h() {
}

/* g becomes STRUB_INTERNAL because of the flag, and gets split into
   STRUB_WRAPPER and STRUB_WRAPPED.  */
static inline void
g() {
  h();
}

/* f becomes STRUB_INTERNAL because of the flag, and gets split into
   STRUB_WRAPPER and STRUB_WRAPPED.  */
void
f() {
  g();
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 3 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]callable\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 2 "strubm" } } */

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 4 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapped\[)\]" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]wrapper\[)\]" 2 "strub" } } */
