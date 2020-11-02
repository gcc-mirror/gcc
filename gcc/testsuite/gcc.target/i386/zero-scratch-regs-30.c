/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2  -fzero-call-used-regs=all" } */

_Complex long double ret_x87_cplx (void)
{
  return 1.1L + 1.2iL;
}

/* { dg-final { scan-assembler-times "fldz" 8 { target ia32 } } } */
/* { dg-final { scan-assembler-times "fstp" 8 { target ia32 } } } */
/* { dg-final { scan-assembler-times "fldz" 6 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "fstp" 6 { target { ! ia32 } } } } */
