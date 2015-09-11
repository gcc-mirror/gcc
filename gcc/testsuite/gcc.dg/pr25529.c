/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

int
f (unsigned t)
{
  return (t * 2) / 2;
}

/* { dg-final { scan-tree-dump "\& 2147483647" "optimized" } } */
