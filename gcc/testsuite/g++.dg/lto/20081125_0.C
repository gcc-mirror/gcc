// { dg-lto-do link }
// { dg-lto-options {{-flto -flto-partition=1to1}} }
#include "20081125.h"

object::object (int x)
{
}

void
object::key_method (void)
{
}

int
main ()
{
  return 0;
}
