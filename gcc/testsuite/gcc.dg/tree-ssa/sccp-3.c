/* PR/113080 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int a,b,n;
int w;
void fun1(int t)
{
  for(int i=0;i<100;i++)
    {
      a+=w;
      b-=w;
      t+=a+b;
    }
  n=t;
}

/* We should apply final value replacement to all reductions and
   elide the loop.  */
/* { dg-final { scan-tree-dump-times "<bb" 1 "optimized" } } */
