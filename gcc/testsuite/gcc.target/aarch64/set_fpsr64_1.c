/* { dg-do compile } */
/* { dg-options "-O2" } */

void
set_fpsr64 (long long unsigned x)
{
  return __builtin_aarch64_set_fpsr64 (x);
}

/* { dg-final { scan-assembler-times {\tmsr\tfpsr, x0\n} 1 } } */
