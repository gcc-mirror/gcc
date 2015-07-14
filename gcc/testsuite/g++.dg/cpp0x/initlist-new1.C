// { dg-do compile { target c++11 } }

struct A
{
  A() = default;
  A(const A&) = default;
};

void f()
{
  new A{A()};
}
