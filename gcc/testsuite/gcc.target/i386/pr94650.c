/* PR target/94650 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#define LARGE_POWER_OF_TWO (1ULL << 40)

int
check (unsigned long long m)
{
  return m >= LARGE_POWER_OF_TWO;
}

void g (int);

void
test0 (unsigned long long m)
{
  if (m >= LARGE_POWER_OF_TWO)
    g (0);
}

void
test1 (unsigned long long m)
{
  if (m >= LARGE_POWER_OF_TWO)
    g (m);
}

/* { dg-final { scan-assembler-not "movabs" } } */
/* { dg-final { scan-assembler-times "shr" 3 } } */
