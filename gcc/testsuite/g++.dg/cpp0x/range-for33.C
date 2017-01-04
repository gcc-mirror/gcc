// PR c++/77284
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A
{
  ~A () {}
};

void foo (A & v)
{
  for (A a : { v }) {};
}
