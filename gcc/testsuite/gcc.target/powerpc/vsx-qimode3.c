/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mvsx-small-integer" } */

double load_asm_v_constraint (signed char *p)
{
  double ret;
  __asm__ ("xxlor %x0,%x1,%x1\t# load v constraint" : "=d" (ret) : "v" (*p));
  return ret;
}

void store_asm_v_constraint (signed char *p, double x)
{
  signed char i;
  __asm__ ("xxlor %x0,%x1,%x1\t# store v constraint" : "=v" (i) : "d" (x));
  *p = i;
}

/* { dg-final { scan-assembler "lxsibzx" } } */
/* { dg-final { scan-assembler "stxsibx" } } */
