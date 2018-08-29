// { dg-do compile { target c++11 } }

template <int I> void f();

struct A { constexpr operator int() { return 24; } };

template <class T> constexpr void g(T t)
{
  f<t>();
}

int main()
{
  g(A());
}
