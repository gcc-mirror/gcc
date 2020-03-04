// PR c++/90505 - mismatch in template argument deduction.
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
  template <typename U = int, typename V>
  static void foo(V) { }

  void bar () { foo<>(10); }
};

void
test ()
{
  S<int> s;
  s.bar ();
}
