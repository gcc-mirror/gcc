/* { dg-do compile } */
/* { dg-additional-options "-fgimple -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_double } */

double a[2];

void __GIMPLE (ssa,startwith ("fix_loops"))
foo (double x)
{
  double tem2;
  double tem1;
  double _1;
  double _2;
  double _3;
  double _4;

  __BB(2):
  _1 = a[0];
  _2 = x_6(D) * 3.0e+0;
  tem1_7 = _1 + _2;
  _3 = x_6(D) + 1.0e+0;
  _4 = a[1];
  tem2_8 = _4 + _3;
  a[0] = tem1_7;
  a[1] = tem2_8;
  return;
}

void __GIMPLE (ssa,startwith ("fix_loops"))
bar (double x)
{
  double tem2;
  double tem1;
  double _1;
  double _2;
  double _3;
  double _4;

  __BB(2):
  _1 = a[0];
  _2 = x_6(D) * 3.0e+0;
  tem1_7 = _1 + _2;
  _3 = x_6(D) + 1.0e+0;
  _4 = a[1];
  /* Once with operands swapped.  */
  tem2_8 = _3 + _4;
  a[0] = tem1_7;
  a[1] = tem2_8;
  return;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 2 "slp2" } } */
/* We want to vectorize as { a[0], a[1] } + { x*3, x+1 } and thus
   elide one add in each function.  */
/* { dg-final { scan-tree-dump-times " \\+ " 4 "optimized" } } */
