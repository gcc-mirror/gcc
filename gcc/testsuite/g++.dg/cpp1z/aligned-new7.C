// PR c++/77742
// { dg-do compile { target c++17 } }
// { dg-options "-Wall" }

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
