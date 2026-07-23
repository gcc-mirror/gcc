/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt4-details" } */
/* PR tree-optmization/100173 */
/* TARGET_MEM_REF was not being supported for load factoring.  */

void f(int a, int *b, int *d, int *c)
{
    for(int i = 0; i < 1024; i++)
    {
      int t;
      int t1;
      if (b[i] > d[i]) {
        t1 = b[i];
        t = c[i];
      }
      else {
        t1 = d[i];
        t = c[i+2];
      }
      b[i] = t+t1;
    }
}

/* { dg-final { scan-tree-dump "changed to factor out load from COND_EXPR" "phiopt4" } } */
