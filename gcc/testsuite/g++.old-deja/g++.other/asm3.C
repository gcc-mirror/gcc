// { dg-do assemble { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2" }

typedef unsigned long long uint64;
uint64 fstps(void)
{
  uint64 ret;
  asm volatile("fstps %0" : "=m" (ret));
  return ret;
}
