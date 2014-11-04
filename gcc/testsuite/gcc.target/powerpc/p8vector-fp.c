/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mupper-regs-df -mupper-regs-sf -fno-math-errno" } */

float abs_sf (float *p)
{
  float f = *p;
  __asm__ ("# reg %x0" : "+v" (f));
  return __builtin_fabsf (f);
}

float nabs_sf (float *p)
{
  float f = *p;
  __asm__ ("# reg %x0" : "+v" (f));
  return - __builtin_fabsf (f);
}

float neg_sf (float *p)
{
  float f = *p;
  __asm__ ("# reg %x0" : "+v" (f));
  return - f;
}

float add_sf (float *p, float *q)
{
  float f1 = *p;
  float f2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (f1), "+v" (f2));
  return f1 + f2;
}

float sub_sf (float *p, float *q)
{
  float f1 = *p;
  float f2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (f1), "+v" (f2));
  return f1 - f2;
}

float mul_sf (float *p, float *q)
{
  float f1 = *p;
  float f2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (f1), "+v" (f2));
  return f1 * f2;
}

float div_sf (float *p, float *q)
{
  float f1 = *p;
  float f2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (f1), "+v" (f2));
  return f1 / f2;
}

float sqrt_sf (float *p)
{
  float f = *p;
  __asm__ ("# reg %x0" : "+v" (f));
  return __builtin_sqrtf (f);
}


double abs_df (double *p)
{
  double d = *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return __builtin_fabs (d);
}

double nabs_df (double *p)
{
  double d = *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return - __builtin_fabs (d);
}

double neg_df (double *p)
{
  double d = *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return - d;
}

double add_df (double *p, double *q)
{
  double d1 = *p;
  double d2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (d1), "+v" (d2));
  return d1 + d2;
}

double sub_df (double *p, double *q)
{
  double d1 = *p;
  double d2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (d1), "+v" (d2));
  return d1 - d2;
}

double mul_df (double *p, double *q)
{
  double d1 = *p;
  double d2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (d1), "+v" (d2));
  return d1 * d2;
}

double div_df (double *p, double *q)
{
  double d1 = *p;
  double d2 = *q;
  __asm__ ("# reg %x0, %x1" : "+v" (d1), "+v" (d2));
  return d1 / d2;
}

double sqrt_df (float *p)
{
  double d = *p;
  __asm__ ("# reg %x0" : "+v" (d));
  return __builtin_sqrt (d);
}

/* { dg-final { scan-assembler "xsabsdp"  } } */
/* { dg-final { scan-assembler "xsadddp"  } } */
/* { dg-final { scan-assembler "xsaddsp"  } } */
/* { dg-final { scan-assembler "xsdivdp"  } } */
/* { dg-final { scan-assembler "xsdivsp"  } } */
/* { dg-final { scan-assembler "xsmuldp"  } } */
/* { dg-final { scan-assembler "xsmulsp"  } } */
/* { dg-final { scan-assembler "xsnabsdp" } } */
/* { dg-final { scan-assembler "xsnegdp"  } } */
/* { dg-final { scan-assembler "xssqrtdp" } } */
/* { dg-final { scan-assembler "xssqrtsp" } } */
/* { dg-final { scan-assembler "xssubdp"  } } */
/* { dg-final { scan-assembler "xssubsp"  } } */
