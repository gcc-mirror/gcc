// PR c++/99806
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct S {
  void f(auto, auto, int = 3);
  void f2(auto, auto, int = 3) { }
  template<typename T> static T g(T, auto, int = 3);
};

void
g ()
{
  S::g(1, 2);
  S s;
  s.f(1, 2);
  s.f2(1, 2);
}
