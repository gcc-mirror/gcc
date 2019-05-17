#include "pr90326.H"

int main()
{
  float f = __FLT_MAX__;
  if (f == 0.0)
    __builtin_abort ();
  return 0;
}
