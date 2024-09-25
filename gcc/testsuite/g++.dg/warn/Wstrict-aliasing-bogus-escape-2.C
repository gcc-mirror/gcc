/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing" } */
/* { dg-skip-if "requires hosted libstdc++ for list" { ! hostedlib } } */

#include<list>

struct A
{
  virtual ~A();
};

A* foo();

void bar(std::list<int> x)
{
  std::list<int> y = x;
  if (*y.rbegin())
    delete foo();
}

