// { dg-options "-std=c++0x" }

auto f() -> int
{
  return 0;
}

template<class T, class U>
auto add(T t, U u) -> decltype (t+u)
{
  return t+u;
}

template<class T, class U>
decltype(T()+U()) add2(T t, U u);

template <class T, class U>
U g (T, U);

template<class T, class U>
auto add3(T t, U u) -> decltype (g(t,u));

int main()
{
  auto i = add(1, 2.0);
  auto i2 = add2(1, 2.0);
  auto i3 = add3(1, 2.0);
}
