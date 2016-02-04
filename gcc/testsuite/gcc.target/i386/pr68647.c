/* PR target/68647 */
/* { dg-do compile } */
/* { dg-options "-O2 -mpopcnt" } */

int
f1 (unsigned long long a)
{
  return __builtin_popcountll (a);
}

int
f2 (unsigned long long a)
{
  return __builtin_parityll (a);
}

/* { dg-final { scan-assembler-not "__popcountdi2" } } */
/* { dg-final { scan-assembler-not "__paritydi2" } } */
