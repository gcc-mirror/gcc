#include "comdat3.h"

void f ()
{
  const bool *p = &A<int>::b;
}
