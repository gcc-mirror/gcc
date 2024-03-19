/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* The pointer itself is a strub variable, enabling internal strubbing when
   its value is used.  */
int __attribute__ ((__strub__)) *ptr;

int *f() {
  return ptr;
}

/* { dg-final { scan-ipa-dump "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_update" "strub" } } */
