// PR c++/26036
// Origin: <ben@pc-doctor.com>
// { dg-do compile }

struct A
{
  int i;
};

A foo(int);       // { dg-error "too few arguments" }

int j = foo().i;  // { dg-error "at this point" }
