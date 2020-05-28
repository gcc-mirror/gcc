/* { dg-do compile } */
/* { dg-options "-O2" } */

long long unsigned
get_fpsr64 ()
{
  return __builtin_aarch64_get_fpsr64 ();
}

/* { dg-final { scan-assembler-times {\tmrs\tx0, fpsr\n} 1 } } */
