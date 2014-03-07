// { dg-do compile { target c++11 } }

#include <initializer_list>
#define SA(X) static_assert(X,#X)

constexpr int f(std::initializer_list<int> l) { return l.begin()[0]; }

int main()
{
  constexpr int i = f({42});
  SA(i==42);
}
