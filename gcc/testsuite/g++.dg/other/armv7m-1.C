/* { dg-do run { target arm*-*-* } } */
/* Test Armv7m interrupt routines.  */
#include <stdlib.h>

#ifdef __ARM_ARCH_7M__
void __attribute__((interrupt))
foo(void)
{
  long long n;
  long p;
  asm volatile ("" : "=r" (p) : "0" (&n));
  if (p & 4)
    abort ();
  return;
}

void __attribute__((interrupt))
bar(void)
{
  throw 42;
}

int main()
{
  int a;
  int before;
  int after;
  volatile register int sp asm("sp");

  asm volatile ("mov %0, sp\n"
		"blx %2\n"
		"mov %1, sp\n"
		: "=&r" (before), "=r" (after) : "r" (foo)
		: "memory", "cc", "r0", "r1", "r2", "r3", "ip", "lr");
  if (before != after)
    abort();
  asm volatile ("mov %0, sp\n"
		"sub sp, sp, #4\n"
		"blx %2\n"
		"add sp, sp, #4\n"
		"mov %1, sp\n"
		: "=&r" (before), "=r" (after) : "r" (foo)
		: "memory", "cc", "r0", "r1", "r2", "r3", "ip", "lr");
  if (before != after)
    abort();
  before = sp;
  try
    {
      bar();
    }
  catch (int i)
    {
      if (i != 42)
	abort();
    }
  catch (...)
    {
      abort();
    }
  if (before != sp)
    abort();
  exit(0);
}
#else
int main()
{
  exit (0);
}
#endif
