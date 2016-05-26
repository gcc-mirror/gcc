/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2 -mpower9-minmax -ffast-math" } */
/* { dg-final { scan-assembler-not "fsel"      } } */
/* { dg-final { scan-assembler     "xscmpeqdp" } } */
/* { dg-final { scan-assembler     "xscmpgtdp" } } */
/* { dg-final { scan-assembler     "xscmpgedp" } } */
/* { dg-final { scan-assembler-not "xscmpodp"  } } */
/* { dg-final { scan-assembler-not "xscmpudp"  } } */
/* { dg-final { scan-assembler     "xsmaxcdp"  } } */
/* { dg-final { scan-assembler-not "xsmaxdp"   } } */
/* { dg-final { scan-assembler     "xsmincdp"  } } */
/* { dg-final { scan-assembler-not "xsmindp"   } } */
/* { dg-final { scan-assembler     "xxsel"     } } */

double
dbl_max1 (double a, double b)
{
  return (a >= b) ? a : b;
}

double
dbl_max2 (double a, double b)
{
  return (a > b) ? a : b;
}

double
dbl_min1 (double a, double b)
{
  return (a < b) ? a : b;
}

double
dbl_min2 (double a, double b)
{
  return (a <= b) ? a : b;
}

double
dbl_cmp_eq (double a, double b, double c, double d)
{
  return (a == b) ? c : d;
}

double
dbl_cmp_ne (double a, double b, double c, double d)
{
  return (a != b) ? c : d;
}

double
dbl_cmp_gt (double a, double b, double c, double d)
{
  return (a > b) ? c : d;
}

double
dbl_cmp_ge (double a, double b, double c, double d)
{
  return (a >= b) ? c : d;
}

double
dbl_cmp_lt (double a, double b, double c, double d)
{
  return (a < b) ? c : d;
}

double
dbl_cmp_le (double a, double b, double c, double d)
{
  return (a <= b) ? c : d;
}

float
flt_max1 (float a, float b)
{
  return (a >= b) ? a : b;
}

float
flt_max2 (float a, float b)
{
  return (a > b) ? a : b;
}

float
flt_min1 (float a, float b)
{
  return (a < b) ? a : b;
}

float
flt_min2 (float a, float b)
{
  return (a <= b) ? a : b;
}

float
flt_cmp_eq (float a, float b, float c, float d)
{
  return (a == b) ? c : d;
}

float
flt_cmp_ne (float a, float b, float c, float d)
{
  return (a != b) ? c : d;
}

float
flt_cmp_gt (float a, float b, float c, float d)
{
  return (a > b) ? c : d;
}

float
flt_cmp_ge (float a, float b, float c, float d)
{
  return (a >= b) ? c : d;
}

float
flt_cmp_lt (float a, float b, float c, float d)
{
  return (a < b) ? c : d;
}

float
flt_cmp_le (float a, float b, float c, float d)
{
  return (a <= b) ? c : d;
}

double
dbl_flt_max1 (float a, float b)
{
  return (a > b) ? a : b;
}

double
dbl_flt_max2 (double a, float b)
{
  return (a > b) ? a : b;
}

double
dbl_flt_max3 (float a, double b)
{
  return (a > b) ? a : b;
}

double
dbl_flt_min1 (float a, float b)
{
  return (a < b) ? a : b;
}

double
dbl_flt_min2 (double a, float b)
{
  return (a < b) ? a : b;
}

double
dbl_flt_min3 (float a, double b)
{
  return (a < b) ? a : b;
}
