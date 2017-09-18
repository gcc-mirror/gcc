/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

unsigned long long test (unsigned long long a)
{
  return a | (1ull << 55);
}

extern unsigned long long m;

void testm (void)
{
  m |= (1ull << 45);
}

/* { dg-final { scan-assembler-times "bts" 2 } } */
