// PR tree-optimization/42625
// { dg-do run }
// { dg-options "-O1 -fipa-sra" }
// { dg-additional-sources "dtor4-aux.cc" }
// { dg-skip-if "requires hosted libstdc++ for cassert in dtor4.h" { ! hostedlib } }

#include "dtor4.h"

int
main ()
{
  S s;
  return 0;
}
