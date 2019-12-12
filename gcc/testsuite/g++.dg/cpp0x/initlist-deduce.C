// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A
{
  A(std::initializer_list<int>) { }
};

void f (A a) { }

template <class T>
auto g (T&& t) -> decltype (f(t))
{
  return f(t);
}

int main()
{
  g({1});			// { dg-error "no matching function" }
}

