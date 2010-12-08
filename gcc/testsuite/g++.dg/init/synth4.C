// PR c++/45822

struct A
{
  A(int);
};

struct B
{
  B(A = 0);
};

struct C
{
  B b;
};

C c;
