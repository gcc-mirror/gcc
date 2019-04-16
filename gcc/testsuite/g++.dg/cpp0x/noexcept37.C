// PR c++/89571
// { dg-do compile { target c++11 } }

struct A
{
  int i = ;  // { dg-error "expected" }
  A() noexcept = default;
};
