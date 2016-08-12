/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2 -mupper-regs-di" } */

vector long
merge (long a, long b)
{
  return (vector long) { a, b };
}

/* { dg-final { scan-assembler "mtvsrdd" } } */
