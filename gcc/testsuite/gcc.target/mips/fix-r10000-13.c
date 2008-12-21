/* { dg-do compile } */
/* { dg-options "-O2 -mfix-r10000" } */
/* { dg-final { scan-assembler-times "\tbeql\t" 3 } } */

NOMIPS16 int
f1 (int *z)
{
  return __sync_bool_compare_and_swap (z, 0, 42);
}

NOMIPS16 short
f2 (short *z)
{
  return __sync_bool_compare_and_swap (z, 0, 42);
}

NOMIPS16 char
f3 (char *z)
{
  return __sync_bool_compare_and_swap (z, 0, 42);
}
