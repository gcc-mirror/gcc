/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned
foo (int x)
{
  return __builtin_popcount (x);
}

unsigned long
foo1 (int x)
{
  return __builtin_popcount (x);
}

/* { dg-final { scan-assembler-not {popcount} } } */
/* { dg-final { scan-assembler-times {cnt\t} 2 } } */
/* { dg-final { scan-assembler-times {fmov} 4 } } */
/* { dg-final { scan-assembler-not {umov} } } */
/* { dg-final { scan-assembler-not {uxtw} } } */
/* { dg-final { scan-assembler-not {sxtw} } } */
