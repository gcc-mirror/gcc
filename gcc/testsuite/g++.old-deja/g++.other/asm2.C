// Build don't link:
// Skip if not target: i?86-*-*
// Special g++ Options: -O2

typedef unsigned long long uint64;
uint64 fstps(void)
{
  uint64 ret;
  asm volatile("fstps %0" : "=m" (ret));
  return ret;
}
