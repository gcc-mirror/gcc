/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* The pointer itself is a strub variable, that would enable internal strubbing
   if its value was used.  Here, it's only overwritten, so no strub.  */
int __attribute__ ((__strub__)) *ptr;

void f() {
  ptr = 0;
}

/* { dg-final { scan-ipa-dump-not "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump-not "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump-not "strub_update" "strub" } } */
