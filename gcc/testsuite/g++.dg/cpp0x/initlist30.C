// Testcase for variadic init list deduction.
// { dg-do compile { target c++11 } }

#include <initializer_list>

template <class... Ts>
void f (std::initializer_list<Ts>... ls);

int main()
{
  f({1},{2.0});
}
