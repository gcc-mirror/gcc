/* { dg-do compile } */
/* { dg-options "-march=octeon -mbranch-likely -fno-unroll-loops" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tbbit\[01\]\t" } } */
/* { dg-final { scan-assembler-not "\tbbit\[01\]l\t" } } */
/* { dg-final { scan-assembler "\tbnel\t" } } */

NOMIPS16 int
f (int *a, int *b)
{
  do
    if (__builtin_expect (*a & 1, 1))
      *a = 0;
  while (++a < b);
}

NOMIPS16 int
g (int *a, int *b)
{
  do
    if (__builtin_expect (*a == 3, 1))
      *a = 0;
  while (++a < b);
}
