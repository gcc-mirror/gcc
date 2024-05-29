/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -ftree-vectorize -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "xvaddsp" } } */
/* { dg-final { scan-assembler "xvsubsp" } } */
/* { dg-final { scan-assembler "xvmulsp" } } */
/* { dg-final { scan-assembler "xvdivsp" } } */
/* { dg-final { scan-assembler "xvmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */

__vector float a, b, c, d;

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
