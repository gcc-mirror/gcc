/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

void scrub_int (int *p)
{
  *p = 0;
  *(volatile int *)p = 0;
}

/* { dg-final { scan-tree-dump-not " = 0;" "optimized" } } */
/* { dg-final { scan-tree-dump " ={v} 0;" "optimized" } } */
