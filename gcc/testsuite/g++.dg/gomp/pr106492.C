/* PR middle-end/106492 */

template <typename T>
struct S {
  T a : 12;
  S () : a(0)
  {
#pragma omp for simd linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};
struct U {
  int a : 12;
  U () : a(0)
  {
#pragma omp for simd linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};

S<int> s;
U u;


template <typename T>
struct Sptr {
  T a;
  Sptr (T init) : a(init)
  {
#pragma omp for simd linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};
struct Uptr {
  int *a;
  Uptr (int *init) : a(init)
  {
#pragma omp for simd linear(a)
    for (int k = 0; k < 64; ++k)
      a++;
  }
};

int i[1024];
Sptr<int *> sptr(i);
Uptr uptr(&i[100]);
