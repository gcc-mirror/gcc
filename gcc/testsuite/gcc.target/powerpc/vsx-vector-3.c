/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -ftree-vectorize -mcpu=power7" } */
/* { dg-final { scan-assembler "xvadddp" } } */
/* { dg-final { scan-assembler "xvsubdp" } } */
/* { dg-final { scan-assembler "xvmuldp" } } */
/* { dg-final { scan-assembler "xvdivdp" } } */
/* { dg-final { scan-assembler "xvmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */

__vector double a, b, c, d;

void
vector_add (void)
{
  a = b + c;
}

void
vector_subtract (void)
{
  a = b - c;
}

void
vector_multiply (void)
{
  a = b * c;
}

void
vector_multiply_add (void)
{
  a = (b * c) + d;
}

void
vector_multiply_subtract (void)
{
  a = (b * c) - d;
}

void
vector_divide (void)
{
  a = b / c;
}
