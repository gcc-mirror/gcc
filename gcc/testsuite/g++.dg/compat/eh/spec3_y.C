#include "spec3.h"

A::A() {}

void func()
#if __cplusplus < 201103L
throw (B,A)
#endif
{
  throw A();
}
