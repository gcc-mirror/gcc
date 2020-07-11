// PR c++/95063

template <typename T>
struct S {
  T a : 12;
  S () : a(0)
  {
#pragma omp for linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};
struct U {
  int a : 12;
  U () : a(0)
  {
#pragma omp for linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};

S<int> s;
U u;
