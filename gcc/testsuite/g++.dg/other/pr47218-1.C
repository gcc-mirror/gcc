#include "pr47218.h"

Foo2::~Foo2 ()
{
  ((FooBaseBase1*)this)->Bar();
}

void Foo2::Bar()
{
}
