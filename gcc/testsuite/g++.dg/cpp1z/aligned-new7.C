// PR c++/77742
// { dg-options "-Wall -std=c++1z" }

#include <new>

struct X
{
  alignas(2*__STDCPP_DEFAULT_NEW_ALIGNMENT__) int i;
};

int main()
{
  alignas(alignof(X)) char buf[sizeof(X)];
  ::new((void*)buf) X{1};
}
