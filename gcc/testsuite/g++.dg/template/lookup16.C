// PR c++/94799
// { dg-do compile { target c++11 } }

template <typename> struct A {
  typedef int type;
  operator int();
};

template <typename T> using B = A<T>;

template <typename T> typename B<T>::type foo(B<T> b)
{
  auto r1 = b.operator typename A<T>::type();
  auto r2 = b.operator typename A<T>::template A<T>::type();
  auto r3 = b.operator typename B<T>::type();
  auto r4 = b.operator typename B<T>::template A<T>::type();
  return r1 + r2 + r3 + r4;
}

void bar()
{
  foo(A<int>());
}
