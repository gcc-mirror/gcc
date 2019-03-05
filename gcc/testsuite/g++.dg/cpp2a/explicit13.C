// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<int M = 0> struct A {
  template<typename T, int N = 0>
  explicit(N + M) operator T();
};

template<int M = 1> struct B {
  template<typename T, int N = 1>
  explicit(N * M) operator T();
};

void
bar ()
{
  A a;
  int i = a;

  A<0> a0;
  int i0 = a0;

  A<1> a1;
  int i1 = a1; // { dg-error "cannot convert" }

  B b;
  int j = b; // { dg-error "cannot convert" }

  B<0> b0;
  int j0 = b0;

  B<1> b1;
  int j1 = b1; // { dg-error "cannot convert" }
}
