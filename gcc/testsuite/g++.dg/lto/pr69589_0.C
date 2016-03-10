// { dg-lto-do link }
// { dg-lto-options "-O2 -rdynamic" } 
// { dg-extra-ld-options "-r -nostdlib" }
#pragma GCC visibility push(hidden)
struct A { int &operator[] (long); };
template <typename> struct B;
template <typename T, typename = B<T> >
using Z = int;
template <typename> struct C;
struct S {
  int e;
  virtual ~S () {}
};
struct D : S {
  A a;
  long i;
  D() { { e ? &a[i] : nullptr; } }
};
template <>
struct C<int> { Z<S> m8 () const; };
Z<S>
C<int>::m8 () const
{
  D ();
}

