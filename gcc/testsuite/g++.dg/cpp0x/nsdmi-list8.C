// PR c++/104583
// { dg-do compile { target c++11 } }

struct A {
  A();
  int c;
};

struct D {
  A a{};
};

void g()
{
  D d;
  d = {};
}
