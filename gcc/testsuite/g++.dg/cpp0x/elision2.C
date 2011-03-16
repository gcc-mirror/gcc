// Core 1148: should be able to move from value parameter on return
// { dg-options -std=c++0x }

struct A
{
  A(const A&) = delete;
  A(A&&);
};

A f (A a)
{
  return a;
}
