/* { dg-options "-march=allegrex -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
/* { dg-final { scan-assembler "\tmaddu\t" } } */

typedef unsigned int ui;
typedef unsigned long long ull;

NOMIPS16 ull
f1 (ui x, ui y, ull z)
{
  return (ull) x * y + z;
}

NOMIPS16 ull
f2 (ui x, ui y, ull z)
{
  return z + (ull) y * x;
}

