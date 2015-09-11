/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-dse1" } */

struct ints { int a, b, c; } foo();
void bar(int a, int b);

void func() {
  volatile struct ints s = foo();
  bar(s.a, s.b);
}
/* { dg-final { scan-rtl-dump "global deletions = 0"  "dse1" } } */
