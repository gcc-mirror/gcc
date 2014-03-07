// { dg-do run { target c++11 } }

// Test typeid

#include <typeinfo>

int main()
{
  const decltype(nullptr) mynull = 0;
  if (typeid(nullptr) != typeid(mynull))
    __builtin_abort();
}
