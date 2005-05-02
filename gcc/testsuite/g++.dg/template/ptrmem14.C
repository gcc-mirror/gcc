// PR c++/15875

struct A
{
  void foo();
};

template<int> void bar()
{
  typedef void (A::*fptr)();
  fptr ptr = &A::foo;
}
