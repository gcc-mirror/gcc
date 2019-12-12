// PR c++/90590
// { dg-options -Wswitch }
#include "pr90590-1.h"

void
g ()
{
  enum E e = _A;
  switch (e) // { dg-bogus "enumeration value '_C' not handled in switch" }
    {
    case _A:
    case _B:
      break;
    }
}
