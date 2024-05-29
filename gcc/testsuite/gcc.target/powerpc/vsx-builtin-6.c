/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Check whether tdiv and tsqrt instructions generate the correct code.  */
/* Each of the *tdiv* and *tsqrt* instructions should be generated exactly 3
   times (the two calls in the _1 function should be combined).  */
/* { dg-final { scan-assembler-times "xstdivdp" 3 } } */
/* { dg-final { scan-assembler-times "xvtdivdp" 3 } } */
/* { dg-final { scan-assembler-times "xvtdivsp" 3 } } */
/* { dg-final { scan-assembler-times "xstsqrtdp" 3 } } */
/* { dg-final { scan-assembler-times "xvtsqrtdp" 3 } } */
/* { dg-final { scan-assembler-times "xvtsqrtsp" 3 } } */

void test_div_df_1 (double a, double b, int *p)
{
  p[0] = __builtin_vsx_xstdivdp_fe (a, b);
  p[1] = __builtin_vsx_xstdivdp_fg (a, b);
}

int *test_div_df_2 (double a, double b, int *p)
{
  if (__builtin_vsx_xstdivdp_fe (a, b))
    *p++ = 1;
  
  return p;
}

int *test_div_df_3 (double a, double b, int *p)
{
  if (__builtin_vsx_xstdivdp_fg (a, b))
    *p++ = 1;

  return p;
}

void test_sqrt_df_1 (double a, int *p)
{
  p[0] = __builtin_vsx_xstsqrtdp_fe (a);
  p[1] = __builtin_vsx_xstsqrtdp_fg (a);
}

int *test_sqrt_df_2 (double a, int *p)
{
  if (__builtin_vsx_xstsqrtdp_fe (a))
    *p++ = 1;

  return p;
}

int *test_sqrt_df_3 (double a, int *p)
{
  if (__builtin_vsx_xstsqrtdp_fg (a))
    *p++ = 1;

  return p;
}

void test_div_v2df_1 (__vector double *a, __vector double *b, int *p)
{
  p[0] = __builtin_vsx_xvtdivdp_fe (*a, *b);
  p[1] = __builtin_vsx_xvtdivdp_fg (*a, *b);
}

int *test_div_v2df_2 (__vector double *a, __vector double *b, int *p)
{
  if (__builtin_vsx_xvtdivdp_fe (*a, *b))
    *p++ = 1;

  return p;
}

int *test_div_v2df_3 (__vector double *a, __vector double *b, int *p)
{
  if (__builtin_vsx_xvtdivdp_fg (*a, *b))
    *p++ = 1;

  return p;
}

void test_sqrt_v2df_1 (__vector double *a, int *p)
{
  p[0] = __builtin_vsx_xvtsqrtdp_fe (*a);
  p[1] = __builtin_vsx_xvtsqrtdp_fg (*a);
}

int *test_sqrt_v2df_2 (__vector double *a, int *p)
{
  if (__builtin_vsx_xvtsqrtdp_fe (*a))
    *p++ = 1;

  return p;
}

int *test_sqrt_v2df_3 (__vector double *a, int *p)
{
  if (__builtin_vsx_xvtsqrtdp_fg (*a))
    *p++ = 1;

  return p;
}

void test_div_v4sf_1 (__vector float *a, __vector float *b, int *p)
{
  p[0] = __builtin_vsx_xvtdivsp_fe (*a, *b);
  p[1] = __builtin_vsx_xvtdivsp_fg (*a, *b);
}

int *test_div_v4sf_2 (__vector float *a, __vector float *b, int *p)
{
  if (__builtin_vsx_xvtdivsp_fe (*a, *b))
    *p++ = 1;

  return p;
}

int *test_div_v4sf_3 (__vector float *a, __vector float *b, int *p)
{
  if (__builtin_vsx_xvtdivsp_fg (*a, *b))
    *p++ = 1;

  return p;
}

void test_sqrt_v4sf_1 (__vector float *a, int *p)
{
  p[0] = __builtin_vsx_xvtsqrtsp_fe (*a);
  p[1] = __builtin_vsx_xvtsqrtsp_fg (*a);
}

int *test_sqrt_v4sf_2 (__vector float *a, int *p)
{
  if (__builtin_vsx_xvtsqrtsp_fe (*a))
    *p++ = 1;

  return p;
}

int *test_sqrt_v4sf_3 (__vector float *a, int *p)
{
  if (__builtin_vsx_xvtsqrtsp_fg (*a))
    *p++ = 1;

  return p;
}
