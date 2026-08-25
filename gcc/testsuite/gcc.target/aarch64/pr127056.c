/* PR rtl-optimization/127056 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

char (*a)[8];
int b;

void
c (void)
{
  if (b >= 0)
    __builtin_prefetch (a[b]);
}

void
d (char (*p)[8], unsigned int i)
{
  __builtin_prefetch (p[i]);
}
