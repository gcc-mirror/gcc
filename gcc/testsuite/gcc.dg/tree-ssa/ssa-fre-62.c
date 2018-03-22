/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

char f()
{
  char *i = (char*)__builtin_malloc(100);
  __builtin_memcpy(i, "a", 1);
  return i[0];
}

/* { dg-final { scan-tree-dump "return 97;" "fre1" } } */
