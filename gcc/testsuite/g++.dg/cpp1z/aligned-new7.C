// PR c++/77742
// { dg-options "-Wall -std=c++17" }

#include <new>

struct X
{
  alignas(2*__STDCPP_DEFAULT_NEW_ALIGNMENT__) int i;
};

alignas(alignof(X)) char buf[sizeof(X)];

int main()
{
  ::new((void*)buf) X{1};
}
