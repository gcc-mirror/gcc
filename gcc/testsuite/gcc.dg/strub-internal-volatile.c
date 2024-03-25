/* { dg-do compile } */
/* { dg-options "-fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

void __attribute__ ((strub("internal")))
f(volatile short s) {
}

void g(void) {
  f(0);
}

/* We make volatile parms indirect in the wrapped f.  */
/* { dg-final { scan-ipa-dump-times "volatile short" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "volatile short int &" 1 "strub" } } */
