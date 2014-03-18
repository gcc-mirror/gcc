// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

class A {
  void bar (void (A::*) (int));
  void foo (int);
  void B ();
};

void A::B()
{
  bar (&A::foo);
}
