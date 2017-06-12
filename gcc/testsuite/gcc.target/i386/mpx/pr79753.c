/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

int
foo (void)
{
  return 0;
}

void
bar (int **p)
{
  *p = (int *) (__UINTPTR_TYPE__) foo ();
}
