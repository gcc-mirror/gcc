/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
// { dg-require-effective-target strub }

extern int __attribute__((__strub__)) initializer ();

int f() {
  static int x = initializer ();
  return x;
}

/* { dg-final { scan-ipa-dump "strub_enter" "strub" } } */
/* { dg-final { scan-ipa-dump "strub_leave" "strub" } } */
/* { dg-final { scan-ipa-dump-not "strub_update" "strub" } } */
