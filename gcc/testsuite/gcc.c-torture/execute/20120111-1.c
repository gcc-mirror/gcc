#include <stdlib.h>
#include <stdint.h>

uint32_t f0a (uint64_t arg2) __attribute__((noinline));

uint32_t
f0a (uint64_t arg)
{
  return ~(arg > -3);
}

int main() {
  uint32_t r1;
  r1 = f0a (12094370573988097329ULL);
  if (r1 != ~0U)
    abort ();
  return 0;
}
