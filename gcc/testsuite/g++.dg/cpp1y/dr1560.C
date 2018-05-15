// Core 1560
// { dg-do compile { target c++14 } }

struct A
{
  A();
  A(const A&) = delete;
};

void f(bool b)
{
  A a;
  b ? a : throw 42;
}
