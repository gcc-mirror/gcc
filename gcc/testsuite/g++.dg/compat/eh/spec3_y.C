#include "spec3.h"

A::A() {}

void func() throw (B,A)
{
  throw A();
}
