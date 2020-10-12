/* { dg-options "-O1" } */

void
fpsr_getter (void)
{
  unsigned int fpsr = __builtin_aarch64_get_fpsr ();
}

void
fpsr64_getter (void)
{
  unsigned long fpsr = __builtin_aarch64_get_fpsr64 ();
}

void
fpcr_getter (void)
{
  unsigned int fpcr = __builtin_aarch64_get_fpcr ();
}

void
fpcr64_getter (void)
{
  unsigned long fpcr = __builtin_aarch64_get_fpcr64 ();
}

/* { dg-final { scan-assembler-times {\tmrs\tx0, fpsr\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmrs\tx0, fpcr\n} 2 } } */
