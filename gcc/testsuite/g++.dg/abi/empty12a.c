#include "empty12.h"
void fun(struct dummy d, struct foo f)
{
  if (f.i1 != -1)
    __builtin_abort();
}
