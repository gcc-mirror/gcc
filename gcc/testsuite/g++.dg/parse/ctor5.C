// PR c++/27309

struct A
{
  int i; // { dg-message "previous" }
  A() i() {}  // { dg-error "declaration" }
};

struct B
{
  A a;
};

B b;
