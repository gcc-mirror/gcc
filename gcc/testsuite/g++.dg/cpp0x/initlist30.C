// Testcase for variadic init list deduction.
// { dg-options "-std=c++0x" }

#include <initializer_list>

template <class... Ts>
void f (std::initializer_list<Ts>... ls);

int main()
{
  f({1},{2.0});
}
