/* { dg-do compile } */
/* { dg-options "-O2 isa_rev>=1 -mgp32" } */
/* { dg-final { scan-assembler-times "\tmaddu\t" 3 } } */

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

NOMIPS16 ull
f3 (ui x, ui y, ull z)
{
  ull t = (ull) x * y;
  int temp = 5;
  if (temp == 5)
    z += t;
  return z;
}
