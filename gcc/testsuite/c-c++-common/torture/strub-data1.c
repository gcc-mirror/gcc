/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* The pointed-to data enables strubbing if accessed.  */
int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-ipa-dump "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_update" "strub" } } */
