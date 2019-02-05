// PR c++/89187
// { dg-do compile { target c++11 } }
// { dg-options "-Os -fno-tree-ccp -fno-tree-sra -fno-inline" }

template <typename T, int N> struct A {
  typedef T __attribute__((vector_size (N))) type;
};
template <typename T, int N> using B = typename A<T, N>::type;
template <typename T> using C = B<T, 4>;
struct D {
  D (C<int> x) : d{x[3]} {}
  D foo () { return d; }
  C<int> d;
};
extern D d;
struct { D bar () { return d; } } l;
struct E { void baz () const; };

void
E::baz () const
{
  l.bar ().foo ();
}
