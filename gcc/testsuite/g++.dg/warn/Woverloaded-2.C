// PR c++/91921 - stray warning with -Woverloaded-virtual.
// { dg-options "-Woverloaded-virtual" }

#include "Woverloaded-2.h"

struct B : A
{
  void f(int);
};
