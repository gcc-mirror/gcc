/* { dg-do compile } */
/* { dg-options "-O2" } */

long long unsigned
get_fpcr64 ()
{
  return __builtin_aarch64_get_fpcr64 ();
}

/* { dg-final { scan-assembler-times {\tmrs\tx0, fpcr\n} 1 } } */
