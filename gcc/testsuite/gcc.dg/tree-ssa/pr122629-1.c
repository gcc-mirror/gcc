/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcvt-details" } */
/* PR tree-optimization/122629 */

typedef int ix4 __attribute__((vector_size(4*sizeof(int))));

int f(ix4 *a, int l, int *b, ix4 *c)
{
  for (int i =0 ;i < l; i++)
  {
    int t;
    ix4 tt = a[i];
    ix4 tt1 = c[i];
    if(*b) t = tt1[0]; else t = tt[0];
    *b = t;
  }
}

int g(ix4 *a, int l, int *b, ix4 *c)
{
  for (int i =0 ;i < l; i++)
  {
    ix4 tt = a[i];
    ix4 tt1 = c[i];
    if(*b) {
        tt = tt1;
        tt[0] = 1;
    } else {
      tt[0] = 1;
    }
    a[i] = tt;
  }
}

/* Make sure BIT_INSERT_EXPR/BIT_FIELD_REF is still factored out for the case if operand 0 is different. */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 2 "ifcvt" } } */
