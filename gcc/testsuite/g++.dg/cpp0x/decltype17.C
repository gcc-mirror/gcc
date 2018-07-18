// PR c++/36628
// { dg-do run { target c++11 } }
// { dg-additional-options "-Wno-return-type" }

#include <typeinfo>
#include <string.h>

int  rvalue();
int& lvalueref();
int&& rvalueref();

decltype(true ? rvalue() : rvalue()) f()
{}

decltype(true ? lvalueref() : lvalueref()) g()
{}

decltype(true ? rvalueref() : rvalueref()) h()
{}

int main()
{
  if (strcmp (typeid(f).name(), "FivE") != 0)
    return 1;
  if (strcmp (typeid(g).name(), "FRivE") != 0)
    return 2;
  if (strcmp (typeid(h).name(), "FOivE") != 0)
    return 3;

  return 0;
}
