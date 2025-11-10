/* { dg-do compile } */
/* PR tree-optimization/122629 */

/* factor_out_operators was factoring out BIT_FIELD_REF and BIT_INSERT_EXPR,
   both which requires constant operands so it would not valid to factor.  */

typedef int ix4 __attribute__((vector_size(4*sizeof(int))));

int f(ix4 *a, int l, int *b)
{
  for (int i =0 ;i < l; i++)
  {
    int t;
    ix4 tt = a[i];
    if(*b) t = tt[1]; else t = tt[0];
    *b = t;
  }
}

int g(ix4 *a, int l, int *b)
{
  for (int i =0 ;i < l; i++)
  {
    ix4 tt = a[i];
    if(*b) tt[1] = 1; else tt[0] = 1;
    *a = tt;
  }
}
