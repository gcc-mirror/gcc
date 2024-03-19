/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strubm" } */
/* { dg-require-effective-target strub } */

static int __attribute__ ((__strub__)) var;

/* g becomes STRUB_INTERNAL because of the use of the strub variable, and gets
   split into STRUB_WRAPPER and STRUB_WRAPPED.  It's not STRUB_AT_CALLS_OPT
   because force_output is set for static non-inline functions when not
   optimizing, and that keeps only_called_directly_p from returning true, which
   makes STRUB_AT_CALLS[_OPT] non-viable.  */
static void
g() {
  var--;
}

/* f becomes STRUB_INTERNAL because of the use of the strub variable, and gets
   split into STRUB_WRAPPER and STRUB_WRAPPED.  */
void
f() {
  var++;
  g();  /* { dg-error "calling non-.strub." } */
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]" 2 "strubm" } } */
/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 2 "strubm" } } */
