/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int x)
{
  return __builtin_popcount (x);
}

long
foo1 (long x)
{
  return __builtin_popcountl (x);
}

long long
foo2 (long long x)
{
  return __builtin_popcountll (x);
}

int
foo3 (int *p)
{
  return __builtin_popcount (*p);
}

/* { dg-final { scan-assembler-not {popcount} } } */
/* { dg-final { scan-assembler-times {cnt\t} 4 } } */
/* { dg-final { scan-assembler-times {fmov\ts} 1 {target lp64} } } */
/* { dg-final { scan-assembler-times {fmov\td} 2 {target lp64} } } */
/* { dg-final { scan-assembler-times {fmov\ts} 2 {target ilp32} } } */
/* { dg-final { scan-assembler-times {fmov\td} 1 {target ilp32} } } */
/* { dg-final { scan-assembler-times {ldr\ts} 1 } } */
