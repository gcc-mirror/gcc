/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

typedef void __attribute__ ((__strub__)) fntype (int, int, ...);
fntype (*ptr);

void f() {
  ptr (0, 0, 1, 1);
}

/* { dg-final { scan-ipa-dump "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump "(0, 0, &\.strub\.watermark\.\[0-9\]\+, 1, 1)" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump-not "strub_update" "strub" } } */
