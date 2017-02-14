extern bool was_f_in_Bar_destroyed;

#include "ctor1.h"

Foo::~Foo()
{
  was_f_in_Bar_destroyed=true;
}

Bar::~Bar()
#if __cplusplus < 201103L
throw(int)
#else
noexcept(false)
#endif
{
  throw 1;
}
