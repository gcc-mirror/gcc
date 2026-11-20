/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt" } */

/* Keep the floating-point value in an FPR when its bits are also used by
   an integer comparison.  Reduced from lgamma.  */

double
f (double x)
{
  if (((union { double d; unsigned long u; }) { x }).u)
    return 0;

  return x * (x * 3);
}

/* The floating-point operations must not reload x from a GPR or a spill
   slot after the integer comparison.  Allow fmv.d.x from zero, which
   constructs the return value of zero.  */
/* { dg-final { scan-assembler-not {\tfmv\.d\.x\t[^,\n]+,(?!zero)} } } */
/* { dg-final { scan-assembler-not {\tfsd\t} } } */
