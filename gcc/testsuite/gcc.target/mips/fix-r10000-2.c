/* { dg-do compile } */
/* { dg-options "-O2 -mfix-r10000" } */
/* { dg-final { scan-assembler-times "\tbeql\t" 3 } } */

NOMIPS16 int
f1 (int *z, int amt)
{
  return __sync_fetch_and_sub (z, amt);
}

NOMIPS16 short
f2 (short *z, short amt)
{
  return __sync_fetch_and_sub (z, amt);
}

NOMIPS16 char
f3 (char *z, char amt)
{
  return __sync_fetch_and_sub (z, amt);
}
