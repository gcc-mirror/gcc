#include "byval1.h"

void *p[2];

int i;
int r;

C::C()  { p[i++] = this; }
C::~C() { if (p[--i] != this) r = 1; }

void Foo (C c)
{
  p[i++] = &c;
}
