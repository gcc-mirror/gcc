/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */

#if defined(__LONG_DOUBLE_IEEE128__)
/* If long double is IEEE 128-bit, we need to use the __ibm128 type instead of
   long double.  We can't use __ibm128 on systems that don't support IEEE
   128-bit floating point, because the type is not enabled on those
   systems.  */
#define LDOUBLE __ibm128

#elif defined(__LONG_DOUBLE_IBM128__)
#define LDOUBLE long double

#else
#error "long double must be either IBM 128-bit or IEEE 128-bit"
#endif

union u_ld { LDOUBLE ld; double d[2]; };

LDOUBLE
pack (double a, double aa)
{
  union u_ld u;
  u.d[0] = a;
  u.d[1] = aa;
  return u.ld;
}

double
unpack_0 (LDOUBLE x)
{
  union u_ld u;
  u.ld = x;
  return u.d[0];
}

double
unpack_1 (LDOUBLE x)
{
  union u_ld u;
  u.ld = x;
  return u.d[1];
}

/* { dg-final { scan-assembler-not "stfd"   } } */
/* { dg-final { scan-assembler-not "lfd"    } } */
/* { dg-final { scan-assembler-not "lxsdx"  } } */
/* { dg-final { scan-assembler-not "stxsdx" } } */
/* { dg-final { scan-assembler-not "mfvsrd" } } */
/* { dg-final { scan-assembler-not "mtvsrd" } } */


