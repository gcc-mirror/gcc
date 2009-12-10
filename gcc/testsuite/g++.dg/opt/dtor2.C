// PR c++/42317
// { dg-do link }
// { dg-options "-O0" }
// { dg-additional-sources "dtor2-aux.cc" }

#include "dtor2.h"

D::D (int x) : C (x) {}

int
main ()
{
}
