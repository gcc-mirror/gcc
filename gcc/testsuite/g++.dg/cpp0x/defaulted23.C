// Test for checking of exception specifications on defaulted fns
// { dg-options -std=c++11 }

struct A
{
  A() noexcept = default;
};

struct B
{
  B() throw (int) = default; // { dg-error "exception-specification that differs from the implicit declaration" }
};

struct C
{
  C() throw (int) { }
};

struct D: C
{
  D() throw (int) = default;
};

struct E
{
  E() = default;
};
