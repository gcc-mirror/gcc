// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<typename> struct A {
  template<typename T, int N = 0>
  explicit(N) operator T();
};

template<typename> struct B {
  template<typename T, int N = 1>
  explicit(N) operator T();
};

void
bar ()
{
  A<int> a;
  int i = a;

  B<int> b;
  int j = b; // { dg-error "cannot convert" }
}
