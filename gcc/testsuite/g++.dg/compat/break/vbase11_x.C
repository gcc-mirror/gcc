#include "vbase11.h"

extern void vbase11_y (derived&);

int base::foo() { return 1; }
int derived::foo() { return 2; }
int derived::bar() { return 3; }

void vbase11_x ()
{
  derived d;

  vbase11_y (d);
}
