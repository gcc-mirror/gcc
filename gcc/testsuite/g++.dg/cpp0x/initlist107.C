// PR c++/89083
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int x[3]; };

template<class T>
decltype(A{1, 2}, T()) fn1(T t)
{
  return t;
}

template<class T>
decltype(A{{1, 2}}, T()) fn2(T t)
{
  return t;
}

void
f()
{
  fn1(1);
  fn2(1);
}
