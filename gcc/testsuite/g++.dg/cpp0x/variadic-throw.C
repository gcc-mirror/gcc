// { dg-options -std=c++0x }
// PR c++/33509
template<int M, int N> struct pair
{
  int i, j;
  pair() : i(M), j(N) {}
};

template<int... M> struct S
{
  template<int... N> static int foo() throw (pair <M, N>...) // { dg-error "mismatched|no matching" }
  {
    return 1;
  }
};

int bar ()
{
  return S<0, 1, 2>::foo<0, 1, 3> ();
}

int wibble()
{
  return S<0, 1, 2>::foo<0, 1> ();
}
