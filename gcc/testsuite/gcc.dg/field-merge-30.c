/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O1 -fdump-tree-ifcombine-details" } */

__attribute__((noipa))
short
cmp3 (short d1, short d2)
{
  short x = d2 ^ d1;
  if (x < 0 || d1 != (d2 & 32767))
    return 0;
  return 1;
}

__attribute__((noipa))
int
cmp_int (int d1, int d2)
{
  int x = d2 ^ d1;
  if (x < 0 || d1 != (d2 & 0x7fffffff))
    return 0;
  return 1;
}

int main ()
{
  if (cmp3 (-32768, -32768) != 0)
    __builtin_abort ();
  if (cmp3 (0, 0) != 1)
    __builtin_abort ();
  if (cmp3 (1234, 1234) != 1)
    __builtin_abort ();
  if (cmp3 (-1, -1) != 0)
    __builtin_abort ();
  if (cmp_int (-0x7fffffff - 1, -0x7fffffff - 1) != 0)
    __builtin_abort ();
  if (cmp_int (0, 0) != 1)
    __builtin_abort ();
  if (cmp_int (123456, 123456) != 1)
    __builtin_abort ();
  if (cmp_int (-1, -1) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "optimizing two comparisons" "ifcombine" } } */
