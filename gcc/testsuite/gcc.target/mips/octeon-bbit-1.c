/* { dg-do compile } */
/* { dg-options "-O2 -march=octeon" } */
/* { dg-final { scan-assembler-times "\tbbit1\t" 4 } } */
/* { dg-final { scan-assembler-times "\tbbit0\t" 2 } } */
/* { dg-final { scan-assembler-not "andi\t" } } */

NOMIPS16 void foo (void);

NOMIPS16 void
f1 (long long i)
{
  if (i & 0x80)
    foo ();
}

NOMIPS16 void
f2 (int i)
{
  if (!(i & 0x80))
    foo ();
}

NOMIPS16 void
f3 (int i)
{
  if (i % 2)
    foo ();
}

NOMIPS16 void
f4 (int i)
{
  if (i & 1)
    foo ();
}

NOMIPS16 void
f5 (long long i)
{
  if ((i >> 3) & 1)
    foo ();
}

unsigned long long r;

NOMIPS16 static inline __attribute__((always_inline)) int
test_bit(unsigned long long nr, const unsigned long long *addr)
{
  return 1UL & (addr[nr >> 6] >> (nr & 63ULL));
}

NOMIPS16 void
f6 ()
{
  if (!test_bit(0, &r))
    foo ();
}
