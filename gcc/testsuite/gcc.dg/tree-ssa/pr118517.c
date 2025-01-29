/* { dg-do compile } */
/* { dg-options "-O1 -fno-ipa-pure-const" } */

void __attribute__((noreturn)) bar(void) {
  __builtin_unreachable ();
}

int p;
void foo() {
  if (p) bar();
}
