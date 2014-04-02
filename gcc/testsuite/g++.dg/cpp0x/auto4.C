// Testcase for deduction of std::initializer_list for auto.
// { dg-do run { target c++11 } }

#include <typeinfo>
#include <initializer_list>
extern "C" void abort();

template <class T>
void f (T t)
{
  auto ilt = { &t, &t };
  if (typeid(ilt) != typeid(std::initializer_list<T*>))
    abort();

  auto il = { 1, 2, 3 };
  if (typeid(il) != typeid(std::initializer_list<int>))
    abort();
}

int main()
{
  auto il = { 1, 2, 3 };
  if (typeid(il) != typeid(std::initializer_list<int>))
    abort();

  f('c');
}
