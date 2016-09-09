// { dg-options -std=c++1z }
// { dg-do run }

#ifndef __STDCPP_DEFAULT_NEW_ALIGNMENT__
#error __STDCPP_DEFAULT_NEW_ALIGNMENT__ not defined
#endif

#include <cstdint>

struct alignas(64) A { int i; };

int main()
{
  A *p = new A;
  if (std::intptr_t(p) % 64 != 0)
    __builtin_abort();
}
