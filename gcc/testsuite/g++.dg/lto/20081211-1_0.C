#include "20081211-1.h"

foo *
create_foo (void)
{
  return new foo;
}

void
destroy_foo (foo *p)
{
  delete p;
}

int
main ()
{
  return 0;
}
