// PR c++/27309

struct A
{
  int i; // { dg-error "conflicts" }
  A() i() {}  // { dg-error "declaration" }
};

struct B
{
  A a;
};

B b;
