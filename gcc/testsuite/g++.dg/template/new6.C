// PR c++/27713
// { dg-do compile }

struct A
{
  template<int> friend void* operator new(__SIZE_TYPE__); // { dg-error "invalid template" }
};
