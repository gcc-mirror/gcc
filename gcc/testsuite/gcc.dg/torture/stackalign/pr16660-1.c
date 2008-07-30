/* { dg-do run } */

void
f ()
{
  unsigned long tmp[4] __attribute__((aligned(16)));
  asm("movaps %%xmm0, (%0)" : : "r" (tmp) : "memory");
}

int
main()
{
  f();
  return 0;
}
