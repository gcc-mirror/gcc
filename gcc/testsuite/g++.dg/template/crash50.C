// PR c++/27398
// { dg-do compile }

struct A
{
  template<int> void* foo(; // { dg-error "expected|initialization|static|template" }
};
