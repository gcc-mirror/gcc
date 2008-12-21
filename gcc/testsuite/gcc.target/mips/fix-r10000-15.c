/* { dg-do compile } */
/* { dg-options "-O2 -mfix-r10000" } */
/* { dg-final { scan-assembler-times "\tbeql\t" 3 } } */

NOMIPS16 int
f1 (int *z)
{
  int result;

  result = __sync_lock_test_and_set (z, 42);
  __sync_lock_release (z);
  return result;
}

NOMIPS16 short
f2 (short *z)
{
  short result;

  result = __sync_lock_test_and_set (z, 42);
  __sync_lock_release (z);
  return result;
}

NOMIPS16 char
f3 (char *z)
{
  char result;

  result = __sync_lock_test_and_set (z, 42);
  __sync_lock_release (z);
  return result;
}
