/* { dg-options "-fno-lto" } */

void
test (int s, int e)
{
  asm volatile ("" : "+g" (s), "+g" (e) : : "memory");
  if (s)
    __builtin_abort ();
}
