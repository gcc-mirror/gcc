// PR c++/14173

struct A;

void foo(const A&);

struct A
{
  A(const A&);
};

struct B
{
  A a;
  A bar() { return a; }
};
