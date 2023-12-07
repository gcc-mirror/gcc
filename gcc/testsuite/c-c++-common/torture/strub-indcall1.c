/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

typedef void __attribute__ ((__strub__)) fntype ();
fntype (*ptr);

void f() {
  ptr ();
}

/* { dg-final { scan-ipa-dump "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump "(&\.strub\.watermark\.\[0-9\]\+)" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump-not "strub_update" "strub" } } */
