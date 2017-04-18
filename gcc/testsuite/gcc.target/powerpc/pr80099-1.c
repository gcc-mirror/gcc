/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mno-upper-regs-sf" } */

/* PR target/80099: compiler internal error if -mno-upper-regs-sf used.  */

int a;
int int_from_mem (vector float *c)
{
  return __builtin_vec_extract (*c, a);
}
