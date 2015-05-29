/* PR tree-optimization/64715 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (char *, int);

void
foo (int x)
{
  char p[16], *q;
  q = p;
  if (x)
    q = p + 3;
  __builtin___strcpy_chk (q, "abcdefghijkl", __builtin_object_size (q, 1));
  bar (p, x);
}

/* { dg-final { scan-tree-dump "__builtin_memcpy \\\(\[^;\n\r\]*, \"abcdefghijkl\", 13\\\);" "optimized" } } */
