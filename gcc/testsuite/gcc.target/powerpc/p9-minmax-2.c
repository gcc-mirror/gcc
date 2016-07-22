/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2 -mpower9-minmax" } */
/* { dg-final { scan-assembler-not "fsel"      } } */
/* { dg-final { scan-assembler     "xscmpeqdp" } } */
/* { dg-final { scan-assembler     "xscmpgtdp" } } */
/* { dg-final { scan-assembler-not "xscmpodp"  } } */
/* { dg-final { scan-assembler-not "xscmpudp"  } } */
/* { dg-final { scan-assembler     "xsmaxcdp"  } } */
/* { dg-final { scan-assembler-not "xsmaxdp"   } } */
/* { dg-final { scan-assembler     "xsmincdp"  } } */
/* { dg-final { scan-assembler-not "xsmindp"   } } */
/* { dg-final { scan-assembler     "xxsel"     } } */

/* Due to NaN support, <= and >= are not handled presently unless -ffast-math
   is used.  At some point this will be fixed and the xscmpgedp instruction can
   be generated normally. The <= and >= tests are bracketed with
   #ifdef DO_GE_LE.  */

#ifdef DO_GE_LE
double
dbl_max1 (double a, double b)
{
  return (a >= b) ? a : b;
}
#endif

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

#ifdef DO_GE_LE
double
dbl_min2 (double a, double b)
{
  return (a <= b) ? a : b;
}
#endif

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

#ifdef DO_GE_LE
double
dbl_cmp_ge (double a, double b, double c, double d)
{
  return (a >= b) ? c : d;
}
#endif

double
dbl_cmp_lt (double a, double b, double c, double d)
{
  return (a < b) ? c : d;
}

#ifdef DO_GE_LE
double
dbl_cmp_le (double a, double b, double c, double d)
{
  return (a <= b) ? c : d;
}
#endif

#ifdef DO_GE_LE
float
flt_max1 (float a, float b)
{
  return (a >= b) ? a : b;
}
#endif

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

#ifdef DO_GE_LE
float
flt_min2 (float a, float b)
{
  return (a <= b) ? a : b;
}
#endif

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

#ifdef DO_GE_LE
float
flt_cmp_ge (float a, float b, float c, float d)
{
  return (a >= b) ? c : d;
}
#endif

float
flt_cmp_lt (float a, float b, float c, float d)
{
  return (a < b) ? c : d;
}

#ifdef DO_GE_LE
float
flt_cmp_le (float a, float b, float c, float d)
{
  return (a <= b) ? c : d;
}
#endif

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
