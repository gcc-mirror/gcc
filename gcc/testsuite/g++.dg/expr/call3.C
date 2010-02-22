// PR c++/26036
// Origin: <ben@pc-doctor.com>
// { dg-do compile }

struct A
{
  int i;
};

A foo(int);       /* { dg-message "note: declared here" } */

int j = foo().i;  // { dg-error "too few arguments" }
