#include "empty6.h"

extern void empty6_y (B&);

void empty6_x ()
{
  B b;

  b.i = 7;

  empty6_y (b);
}
