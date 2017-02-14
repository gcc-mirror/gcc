/* { dg-do compile { target { powerpc64*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

/* This test tests whether -mfloat128-type (which enables the underlying IEEE
   128-bit floating point) is enabled by default on VSX Linux 64-bit systems,
   even if the keywords __float128 and _Float128 (-mfloat128) are not enabled
   via the -mfloat128 switch.  Test that power8 generates a call to the
   __addkf3 emulation function.  */

typedef double          __attribute__((__mode__(__KF__))) f128_t;
typedef _Complex double __attribute__((__mode__(__KC__))) f128c_t;

f128_t
add_scalar (f128_t a, f128_t b)
{
  return a+b;
}


f128c_t
add_complex (f128c_t a, f128c_t b)
{
  return a+b;
}

/* { dg-final { scan-assembler "bl __addkf3" } } */
