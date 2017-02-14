// { dg-options -faligned-new }
// { dg-do run }

#include <new>
#include <stdint.h>

struct A { int i; };

int main()
{
  A* ap = new (std::align_val_t(64)) A;
  if (intptr_t(ap) % 64 != 0)
    __builtin_abort();
}
