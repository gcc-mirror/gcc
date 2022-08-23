/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -fno-tree-loop-distribute-patterns -fdump-tree-ldist-details" } */
/* { dg-final { scan-tree-dump-not "generated strlen" "ldist" } } */

/* Copied from gcc/testsuite/gcc.c-torture/execute/builtins/lib/strlen.c.  */

__SIZE_TYPE__
foo (const char *s)
{
  __SIZE_TYPE__ i;

  i = 0;
  while (s[i] != 0)
    i++;

  return i;
}
