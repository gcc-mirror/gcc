// { dg-do compile }
// { dg-options "-O -fstrict-enums -ftree-vrp -w" }

enum { E0 = 0, E1 = 1, E2 = 2 } e;

int
foo (void)
{
  return __builtin_popcount ((int) e);
}
