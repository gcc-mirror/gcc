/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

void f(unsigned long x);

void g(long x)
{
  f((unsigned long)(-1-x)+1);
}

/* Make sure we do not lose the conversion.  */

/* { dg-final { scan-tree-dump "-\\\(long unsigned int\\\) x" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
