// Core 1148: should be able to move from value parameter on return
// { dg-do compile { target c++11 } }

struct A
{
  A(const A&) = delete;
  A(A&&);
};

A f (A a)
{
  return a;
}
