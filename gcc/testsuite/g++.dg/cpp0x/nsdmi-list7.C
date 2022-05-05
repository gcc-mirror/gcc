// PR c++/93280
// { dg-do compile { target c++11 } }

struct A {
  template <typename T> A(T);
  int c;
};

struct D {
  A a{0};
};

void g()
{
  D d;
  d = {};
}
