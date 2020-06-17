// PR c++/90505 - mismatch in template argument deduction.
// { dg-do compile }

template <typename T>
struct S {
  template <typename U, typename V>
  static void foo(V) { }

  void bar () { foo<int>(10); }
};

void
test ()
{
  S<int> s;
  s.bar ();
}
