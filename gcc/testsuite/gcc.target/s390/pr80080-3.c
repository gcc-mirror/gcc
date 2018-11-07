/* { dg-do compile } */
/* { dg-options "-march=z10" } */

extern int foo3_mem;
int foo3 (void)
{
  return __atomic_exchange_n (&foo3_mem, 5, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler "\n\(\\.L\\d+):\n\tcs\t.*\n\tjne\t\\1\n" } } */
