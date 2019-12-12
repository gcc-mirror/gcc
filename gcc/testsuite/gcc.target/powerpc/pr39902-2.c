/* Check that simplification "x*(-1)" -> "-x" is not performed for decimal
   float types.  */

/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-options "-std=gnu99 -O -mdejagnu-cpu=power6" } */
/* { dg-final { scan-assembler-not "fneg" } } */

extern _Decimal32 a32, b32;
extern _Decimal64 a64, b64;
extern _Decimal128 a128, b128;

void
foo32 (void)
{
  b32 = a32 * -1.0DF;
}

void
foo64 (void)
{
  b64 = a64 * -1.0DD;
}

void
foo128 (void)
{
  b128 = a128 * -1.0DL;
}
