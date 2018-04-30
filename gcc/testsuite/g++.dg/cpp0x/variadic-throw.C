// { dg-do compile { target c++11 } }
// { dg-prune-output "note" }
// PR c++/33509
template<int M, int N> struct pair
{
  int i, j;
  pair() : i(M), j(N) {}
};

template<int... M> struct S
{
  template<int... N> static int foo() throw (pair <M, N>...) // { dg-error "mismatched" "" { target { ! c++17 } } }
  {							     // { dg-error "dynamic exception specification" "" { target c++17 } .-1 }
    return 1;						     // { dg-warning "deprecated" "" { target { ! c++17 } } .-2 }
  }
};

int bar ()
{
  return S<0, 1, 2>::foo<0, 1, 3> ();
}

int wibble()
{
  return S<0, 1, 2>::foo<0, 1> (); // { dg-error "no matching" "" { target { ! c++17 } } }
}
