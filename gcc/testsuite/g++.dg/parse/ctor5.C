// PR c++/27309

struct A
{
  int i;
  A() i() {}  // { dg-error "expected" }
}; // { dg-error "expected" }

struct B
{
  A a;
};

B b;
