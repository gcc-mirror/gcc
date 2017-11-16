// PR c++/28852
// { do-do compile }

struct A
{
  operator int&(int);  // { dg-error "no arguments" }
};

A a;
int& i = a;  // { dg-error "initialization" }
