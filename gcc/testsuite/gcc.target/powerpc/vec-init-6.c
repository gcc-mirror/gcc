/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2" } */

vector int
merge (int a, int b, int c, int d)
{
  return (vector int) { a, b, c, d };
}

/* { dg-final { scan-assembler     "rldicr" } } */
/* { dg-final { scan-assembler     "rldicl" } } */
/* { dg-final { scan-assembler     "mtvsrd" } } */
/* { dg-final { scan-assembler-not "stw"    } } */
/* { dg-final { scan-assembler-not "lxvw4x" } } */
