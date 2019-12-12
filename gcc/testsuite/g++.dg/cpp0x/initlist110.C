// PR c++/89083
// { dg-do compile { target c++11 } }

struct C { int a[3]; int i; };
struct B { C c[3]; };
struct A { B b[3]; };

template<class T, int N>
decltype(A{N, N}, T()) fn1(T t)
{
  return t;
}

template<class T, int N>
decltype(A{{{N, N, N}, {N + 1}}}, T()) fn2(T t)
{
  return t;
}

template<class T, int N, int M>
decltype(A{{N + M}}, T()) fn3(T t)
{
  return t;
}

void
f()
{
  fn1<int, 10>(1);
  fn2<int, 10>(1);
  fn3<int, 10, 20>(1);
}
