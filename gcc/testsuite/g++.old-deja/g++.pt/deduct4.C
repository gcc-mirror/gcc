// { dg-do run  }
// Test that we can deduce t even though T is deduced from a later argument.

template <int I> struct A { };

template <class T, T t> void f (A<t> &, T) { }

int main ()
{
  A<42> a;
  f (a, 24);
}
