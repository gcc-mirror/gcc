/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

unsigned int foo(unsigned int x, int *p)
{
  unsigned int src = x;
  unsigned int dst;
  *p = sizeof (unsigned int);
  __builtin___memcpy_chk (&dst, &src, *p, 16);
  return dst;
}

/* { dg-final { scan-tree-dump "return x" "fre1" } } */
