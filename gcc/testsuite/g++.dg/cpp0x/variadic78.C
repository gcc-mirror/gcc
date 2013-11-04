// PR c++/33496
// { dg-do compile }
// { dg-options "-std=gnu++11" }

template<int M, int N> struct pair
{
  int i, j;
  pair () : i (M), j (N) {}
};

template<int... M> struct S
{
  template<int... N> static int *foo ()
  {
    static int x[] = { (M + N)... };	// { dg-error "mismatched argument pack lengths" }
    return x;
  }
};

int *bar ()
{
  return S<0, 1, 2>::foo<0, 1> ();
}
