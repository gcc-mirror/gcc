/* { dg-do compile } */
/* { dg-options "-march=octeon -mbranch-likely -fno-unroll-loops" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tbbit\[01\]\t" } } */
/* { dg-final { scan-assembler-not "\tbbit\[01\]l\t" } } */
/* { dg-final { scan-assembler "\tbnel\t" } } */
/* { dg-final { scan-assembler-not "\tbne\t" } } */

NOMIPS16 int
f (int n, int i)
{
  int s = 0;
  for (; i & 1; i++)
    s += i;
  return s;
}

NOMIPS16 int
g (int n, int i)
{
  int s = 0;
  for (i = 0; i < n; i++)
    s += i;
  return s;
}
