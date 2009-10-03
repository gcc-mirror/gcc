// { dg-lto-do link }
// { dg-lto-options {{-fwhopr}} }
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
