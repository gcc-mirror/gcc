/* { dg-do compile { target sqrt_insn } } */
/* { dg-options "-fgimple -O2" } */

double __GIMPLE
f1 (double x)
{
  double res;
  res = .SQRT (x);
  return res;
}

void __GIMPLE
f2 (double x)
{
  .SQRT (x); // Dead code
}
