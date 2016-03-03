// { dg-do compile { target c++14 } }

struct A
{
  A(int);
};

struct B
{
  A a{42};
};

B f() { return {}; }
