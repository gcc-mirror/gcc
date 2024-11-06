/* { dg-do compile } */
/* { dg-options "-std=gnu17 -ftrivial-auto-var-init=zero" } */

void bar();

struct A { char d; };
void foo()
{
  struct A e;
  void baz() { bar(e); }
}
