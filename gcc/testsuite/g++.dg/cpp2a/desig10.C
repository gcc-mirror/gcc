// PR c++/71446
// { dg-do compile { target c++11 } }
// { dg-options "" }

#include <initializer_list>

struct S { int value; };

int foo (S);
char *foo (std::initializer_list<int>);

int x = foo ({.value = 0});
