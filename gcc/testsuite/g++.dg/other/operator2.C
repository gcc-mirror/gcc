// PR c++/28852
// { do-do compile }

struct A
{
  operator int&(int);  // { dg-error "3:.A::operator int&\\(int\\). must have no arguments" }
};

A a;
int& i = a;  // { dg-error "initialization" }
