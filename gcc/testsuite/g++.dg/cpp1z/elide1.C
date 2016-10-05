// { dg-options -std=c++1z }

struct A
{
  A();
  A(const A&) = delete;
};

bool b;
A a = A();
A a1 = b ? A() : A();
A a2 = (42, A());

A f();
A a3 = f();
A a4 = b ? A() : f();
