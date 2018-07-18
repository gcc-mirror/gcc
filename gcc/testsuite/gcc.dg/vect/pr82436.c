/* { dg-do compile } */
/* { dg-additional-options "-Ofast -fno-tree-scev-cprop" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

struct reflection_type
{
  int h;
  int k;
  int l;
  double f_exp;
  double f_sigma;
  _Complex double f_calc;
  double f_pred;
  double i_exp;
  double i_sigma;
  double i_pred;
};

double y, w;
int foo (struct reflection_type *r, int n, unsigned s)
{
  int i;
  y = 0;
  w = 0;
  for (i = 1; i < n; ++i)
    {
      struct reflection_type *x = &r[i*s];
      double fpred = x->f_pred;
      double fexp = x->f_exp;
      double tem = (fpred - fexp);
      y += __builtin_fabs (tem / x->f_sigma);
      w += __builtin_fabs (tem / fexp);
    }
  return i;
}
