// { dg-do compile { target c++11 } }

struct A
{
  int i = 0 ? 0 : throw 1;
};


struct B
{
  int f();
  int i = f();
};

struct C
{
  C(int);
};

struct D
{
  C a = 0;
};

A a;
B b;
D d;


