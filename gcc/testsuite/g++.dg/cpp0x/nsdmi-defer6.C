// { dg-do compile { target c++11 } }

struct A
{
  int i = (A(), 42);		// { dg-error "" }
};

A a;
