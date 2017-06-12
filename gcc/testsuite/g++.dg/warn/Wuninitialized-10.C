// { dg-do compile }
// { dg-options "-Wuninitialized" }

struct A
{
  int f,g;

  A()
    {
      f = g; // { dg-warning "g. is used uninitialized" }
    }
};

A a;
