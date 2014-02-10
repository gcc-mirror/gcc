// PR c++/58651
// { dg-require-effective-target c++11 }

struct A
{
  int i;
  A(int j) : i{{j}} {}		// { dg-error "too many braces" }
};

A a(0);
