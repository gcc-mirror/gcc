#include "inline-crossmodule-1.h"
int
main()
{
  struct a a;
  struct b b;
  return a.key () + a.ret1 () + b.ret2() - 3;
}
