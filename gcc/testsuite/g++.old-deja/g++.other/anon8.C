// { dg-do assemble  }

struct B
{
  int a;
  B & operator= (const B &);
};

struct A
{
  union {
    int a;
  };
  B b;
};

A x;

void foo (const A &y)
{
  x = y;
}
