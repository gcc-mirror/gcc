/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void foo (unsigned long);
void
f (unsigned long i)
{
  if ((i & 7) == 6)
    if(i & 1)
      foo (0);
}

/* { dg-final { scan-tree-dump-not "foo"  "optimized" } } */

