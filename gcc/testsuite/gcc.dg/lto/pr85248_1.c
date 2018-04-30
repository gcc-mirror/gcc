/* { dg-options "-fno-lto" } */

__attribute__((__noipa__)) void
test (int s, int e)
{
  asm volatile ("" : "+g" (s), "+g" (e) : : "memory");
  if (s)
    __builtin_abort ();
}
