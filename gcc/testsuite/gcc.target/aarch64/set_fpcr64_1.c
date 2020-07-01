/* { dg-do compile } */
/* { dg-options "-O2" } */

void
set_fpcr64 (long long unsigned x)
{
  return __builtin_aarch64_set_fpcr64 (x);
}

/* { dg-final { scan-assembler-times {\tmsr\tfpcr, x0\n} 1 } } */
