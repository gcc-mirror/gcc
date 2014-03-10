// { dg-do compile { target c++11 } }

template <class Z = void, class T>
void Foo(T)
{
  struct X {};
}

template <class T = int, typename U>
void f(const U&)
{
  auto g = [] () {};
}
