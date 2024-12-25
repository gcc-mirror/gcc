/* glibc bug, https://sourceware.org/ml/libc-alpha/2017-04/msg00256.html
   When using software completions, we have to prevent assembler to match
   input and output operands of sqrtt/sqrtf insn.  Fixed in glibc 2.26.  */
/* { dg-do run } */
/* { dg-options "-fno-builtin-sqrt -mieee" } */

double sqrt (double);

static double
float64frombits (unsigned long b)
{
  union { unsigned long __b; double __d; } u = { .__b = b };
  return u.__d;
}

int
main (void)
{
  double a = float64frombits (2);

  if (sqrt (a) != 3.1434555694052576e-162)
    __builtin_abort ();

  return 0;
}
