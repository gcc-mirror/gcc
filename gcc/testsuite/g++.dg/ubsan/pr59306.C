// { dg-do compile }
// { dg-options "-fsanitize=undefined" }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }

class A {
  void bar (void (A::*) (int));
  void foo (int);
  void B ();
};

void A::B()
{
  bar (&A::foo);
}
