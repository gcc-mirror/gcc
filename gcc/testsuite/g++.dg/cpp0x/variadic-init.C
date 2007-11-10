// { dg-do run }
// { dg-options "-std=gnu++0x" }

// PR c++/33510
#define SIZE_FROM_CTOR
extern "C" void abort ();

template<int M, int N> struct pair
{
  int i, j;
  pair () : i (M), j (N) {}
};

template<int... M> struct S
{
  template<int... N> static int *foo ()
  {
#ifdef SIZE_FROM_CTOR
    static int x[] = { (M + N)..., -1 };
#else
    static int x[1 + sizeof... N] = { (M + N)..., -1 };
#endif
    return x;
  }
};

template<typename... M> struct R
{
  template<typename... N> static int *foo ()
  {
#ifdef SIZE_FROM_CTOR
    static int x[] = { (sizeof(M) + sizeof(N))..., -1 };
#else
    static int x[1 + sizeof... N] = { (sizeof(M) + sizeof(N))..., -1 };
#endif
    return x;
  }
};

int *bar ()
{
  return S<0, 1, 2>::foo<0, 1, 2> ();
}

int *baz ()
{
  return R<char, short, int>::foo<float, double, long> ();
}


int main ()
{
  int *p = bar ();
  if (p[0] != 0 || p[1] != 2 || p[2] != 4 || p[3] != -1)
    abort ();
}
