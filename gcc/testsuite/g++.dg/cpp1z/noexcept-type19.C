// { dg-do compile { target c++11 } }

#include "noexcept-type19.h"

extern "C" void *malloc (size_t);

template<class T> void f(T*);

int main()
{
  f<decltype(malloc)>(operator new);
}
