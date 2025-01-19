// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }
// From <https://github.com/itanium-cxx-abi/cxx-abi/issues/175>.

template <class... T> struct tuple {
  template <unsigned I> T...[I] get();  // { dg-bogus "sorry, unimplemented: mangling" "" { xfail *-*-* } }
};

int
g ()
{
  tuple<int> t;
  return t.get<0>();
}

template<typename T, typename U> concept C = true;
template<typename ...T> struct A {
    template<int I, typename ...U> void f(T...[I], U...[I]) requires C<T...[I], U...[I]>;  // { dg-message "sorry, unimplemented: mangling" }
};

void
h ()
{
  A<char, int, double> a;
  a.f<1, int, int, char>(1, 2);
}
