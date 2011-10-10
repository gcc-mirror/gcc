/* { dg-do link } */
/* { dg-options "-save-temps" } */
/* { dg-additional-sources "pr47218-1.C" } */

#include "pr47218.h"

Foo3::~Foo3 ()
{
  ((FooBaseBase1*)this)->Bar();
}

void Foo3::Bar()
{
}

int main ()
{
  return 0;
}

// { dg-final { cleanup-saved-temps } }
