/* Test SB-1 v2sf extensions.  */
/* { dg-do compile { target mipsisa64*-*-* } } */ 
/* { dg-options "-march=sb1 -O2 -mpaired-single -mhard-float -mfp64 -ffast-math" } */ 
/* { dg-final { scan-assembler "div.ps" } } */ 
/* { dg-final { scan-assembler "recip.ps" } } */ 
/* { dg-final { scan-assembler "sqrt.ps" } } */ 
/* { dg-final { scan-assembler "rsqrt.ps" } } */ 

typedef float v2sf __attribute__ ((vector_size (8)));

v2sf divide (v2sf a, v2sf b)
{
  return a / b;
}

v2sf recip (v2sf a)
{
  return ((v2sf) {1.0, 1.0}) / a;
}

v2sf squareroot (v2sf a)
{
  return __builtin_mips_sqrt_ps (a);
}

v2sf rsqrt (v2sf a)
{
  return ((v2sf) {1.0, 1.0}) / __builtin_mips_sqrt_ps (a);
}
