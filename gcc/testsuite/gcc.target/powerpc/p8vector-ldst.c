/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2 -mupper-regs-df -mupper-regs-sf" } */

float load_sf (float *p)
{
  float f = *p;
  __asm__ ("# reg %x0" : "+v" (f));
  return f;
}

double load_df (double *p)
{
  double d = *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return d;
}

double load_dfsf (float *p)
{
  double d = (double) *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return d;
}

void store_sf (float *p, float f)
{
  __asm__ ("# reg %x0" : "+v" (f));
  *p = f;
}

void store_df (double *p, double d)
{
  __asm__ ("# reg %x0" : "+v" (d));
  *p = d;
}

/* { dg-final { scan-assembler-times "lxsspx"  2 } } */
/* { dg-final { scan-assembler-times "lxsdx"   1 } } */
/* { dg-final { scan-assembler-times "stxsspx" 1 } } */
/* { dg-final { scan-assembler-times "stxsdx"  1 } } */
