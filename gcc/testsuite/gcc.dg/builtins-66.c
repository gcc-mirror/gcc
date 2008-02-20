/* { dg-do compile } */

enum { E0 = 0, E1 = 1, E2 = 2 } e;

int
foo (void)
{
  return __builtin_popcount ((int) e);
}

/* { dg-final { scan-assembler-not "__builtin_popcount" } } */
