/* { dg-do compile } */
/* { dg-options "-O2 -fstrub=strict -fno-exceptions -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that the expected strub calls are issued.
   Tail calls are short-circuited at -O2+.  */

int __attribute__ ((__strub__))
g (int i, int j) {
  return g (i, j);
}

/* { dg-final { scan-ipa-dump-times "strub_enter" 0 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_update" 2 "strub" } } */
/* { dg-final { scan-ipa-dump-times "strub_leave" 0 "strub" } } */
