#if defined (__x86_64__) || (defined (__ia64__) && !defined (__hpux__))
#define FLOAT __float128
#include "fp-cmp-4.c"
#else
int
main ()
{
  return 0;
}
#endif
