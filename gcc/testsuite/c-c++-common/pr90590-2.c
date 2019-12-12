// PR c++/90590
// { dg-options -Wswitch }

#include "pr90590-2.h"

void
fn ()
{
  switch (c.b) // { dg-bogus "enumeration value" }
    ;
}
