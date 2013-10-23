// Test for checking of exception specifications on defaulted fns
// { dg-options -std=c++11 }

struct A
{
  A() noexcept = default;
};

A a;

struct B
{
  B() throw (int) = default; // { dg-message "exception-specification" }
};

B b;				// { dg-error "deleted" }

struct C
{
  C() throw (int) { }
};

C c;

struct D: C
{
  D() throw (int) = default;
};

D d;

struct E
{
  E() = default;
};

E e;
