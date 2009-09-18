// Test for deduction of T as std::initializer_list.  This isn't currently
// supported by the working draft, but is necessary for perfect forwarding
// of initializer-lists to things that can take a std::initializer_list.

// { dg-options -std=c++0x }
// { dg-do run }

#include <initializer_list>

struct A
{
  A(std::initializer_list<int>) { }
};

void f (A a) { }

template <class T>
auto g (T&& t) -> decltype (f(t)) // { dg-warning "call" }
{
  return f(t);
}

int main()
{
  g({1});			// { dg-warning "deduc" }
}
