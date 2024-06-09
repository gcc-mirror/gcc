/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strubm" } */
/* { dg-require-effective-target strub } */

static int __attribute__ ((__strub__)) var;

/* h becomes STRUB_INLINABLE, because of the use of the strub variable,
   and the always_inline flag.  It would get inlined before pass_ipa_strub, if
   it weren't for the error.  */
static inline void
__attribute__ ((__always_inline__))
h() {
  var++;
}

/* g becomes STRUB_AT_CALLS_OPT, because of the use of the strub variable, and
   the viability of at-calls strubbing.  Though internally a strub context, its
   interface is not strub-enabled, so it's not callable from within strub
   contexts.  */
static inline void
g() {
  var--;
  h();
}

/* f becomes STRUB_INTERNAL because of the use of the strub variable, and gets
   split into STRUB_WRAPPER and STRUB_WRAPPED.  */
void
f() {
  var++;
  g();  /* { dg-error "calling non-.strub." } */
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 3 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]inlinable\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]at-calls-opt\[)\]" 1 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 1 "strubm" } } */
