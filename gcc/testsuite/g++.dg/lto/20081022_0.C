#include "20081022.h"

int
f (foo * a)
{
  return a->bar ();
}

int
main()
{
  return 0;
}
