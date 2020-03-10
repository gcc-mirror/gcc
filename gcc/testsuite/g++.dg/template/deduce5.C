// PR c++/90505 - mismatch in template argument deduction.
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
  template <typename U, typename V = void>
  static void foo(U) { }

  void bar () { foo<int>(10); }
};

void
test ()
{
  S<int> s;
  s.bar ();
}
