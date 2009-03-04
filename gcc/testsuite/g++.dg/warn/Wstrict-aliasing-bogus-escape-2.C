/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing" } */

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

