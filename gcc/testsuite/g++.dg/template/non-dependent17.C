// A variant of deduce4.C with multiple overloads of foo.  Verify we don't
// crash after ahead-of-time pruning of the overload set for the non-dependent
// call to foo.
// { dg-do compile }

template <typename T>
struct S {
  template <typename U, typename V>
  static void foo(V) { }
  template <typename U>
  static void foo(...) { }

  void bar () { foo<int>(10); }
};

void
test ()
{
  S<int> s;
  s.bar ();
}
