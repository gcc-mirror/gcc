/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-avx -march=corei7 -fdump-rtl-final" } */

extern const unsigned int a[];
extern const unsigned long long b[];

int
fn1 (unsigned int p1, unsigned long long p2)
{
  unsigned int p3;

  p3 = a[p1];
  if (p3 == 0 || p3 > 64)
    return 0;

  p2 &= b[p1];
  return p2 == ((unsigned long long) 1 << (p3 - 1));
}

// { dg-final { scan-rtl-dump-not "S16 A64\[^\n\]*\\\*movv2di_internal" "final" } }
