/* { dg-do compile } */

unsigned int ui;
unsigned long ul;
unsigned long long ull;

int
f1 ()
{
  return __builtin_popcount (ui);
}

int
f2 ()
{
  return __builtin_popcountl (ul);
}

int
f3 ()
{
  return __builtin_popcountll (ull);
}

/* { dg-final { scan-assembler-times "popcnt\t%r2,%r2,8" 3 } } */
