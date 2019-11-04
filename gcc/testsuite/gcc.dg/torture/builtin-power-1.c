/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in folding of various math "power" functions is
   correctly performed by the compiler.

   Written by Kaveh Ghazi, 2004-03-11.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */
/* { dg-skip-if "PR44214" { *-*-* } { "-O0" } { "" } } */

#include "../builtins-config.h"

#ifdef HAVE_C99_RUNTIME
#define C99CODE(CODE) CODE
#else
#define C99CODE(CODE) 0
#endif

#define PROTOTYPE(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);
#define PROTOTYPE2(FN) extern double FN(double, double); \
  extern float FN##f(float, float); \
  extern long double FN##l(long double, long double);

PROTOTYPE(fabs)
PROTOTYPE(sqrt)
PROTOTYPE(cbrt)
PROTOTYPE2(pow)

void test(double d1, double d2, double d3,
	  float f1, float f2, float f3,
	  long double ld1, long double ld2, long double ld3)
{
  /* Test N1root(N2root(x)) -> pow(x,1/(N1*N2)).  */
  /* E.g. sqrt(cbrt(x)) -> pow(x,1/6).  */
  /* The `ABS' argument is `fabs' when the transformation only works
     for nonnegative arguments.  Otherwise it's blank.  */
#define ROOT_ROOT(FN1,N1,FN2,N2,ABS) \
 extern void link_failure_##FN1##_##FN2(void); \
 if (FN1(FN2(ABS(d1))) != pow(ABS(d1),1.0/(N1*N2)) \
     || C99CODE (FN1##f(FN2##f(ABS(f1))) != powf(ABS(f1),1.0F/(N1*N2))) \
     || C99CODE (FN1##l(FN2##l(ABS(ld1))) != powl(ABS(ld1),1.0L/(N1*N2)))) \
    link_failure_##FN1##_##FN2()

  ROOT_ROOT(sqrt,2,sqrt,2,);
  ROOT_ROOT(sqrt,2,cbrt,3,);
  ROOT_ROOT(cbrt,3,sqrt,2,);
  ROOT_ROOT(cbrt,3,cbrt,3,fabs);

  /* Test pow(Nroot(x),y) -> pow(x,y/N).  */
  /* The `ABS' argument is `fabs' when the transformation only works
     for nonnegative arguments.  Otherwise it's blank.  */
#define POW_ROOT(FN,N,ABS) \
 extern void link_failure_pow_##FN(void); \
 if (pow(FN(ABS(d1)), d2) != pow(ABS(d1),d2/N) \
     || powf(FN##f(ABS(f1)),f2) != powf(ABS(f1),f2/N) \
     || powl(FN##l(ABS(ld1)),ld2) != powl(ABS(ld1),ld2/N)) \
    link_failure_pow_##FN()

  POW_ROOT(sqrt,2,);
  POW_ROOT(cbrt,3,fabs);

  /* Test Nroot(pow(x,y)) -> pow(x,y/N).  */
  /* The `ABS' argument is `fabs' when the transformation only works
     for nonnegative arguments.  Otherwise it's blank.  */
#define ROOT_POW(FN,N,ABS) \
 extern void link_failure_##FN##_pow(void); \
 if (FN(pow(ABS(d1), d2)) != pow(ABS(d1),d2/N) \
     || FN##f(powf(ABS(f1),f2)) != powf(ABS(f1),f2/N) \
     || FN##l(powl(ABS(ld1),ld2)) != powl(ABS(ld1),ld2/N)) \
    link_failure_##FN##_pow()

  ROOT_POW(sqrt,2,fabs);
  ROOT_POW(cbrt,3,fabs);

  /* Test pow(pow(x,y),z) -> pow(x,y*z).  */
#define POW_POW \
 extern void link_failure_pow_pow(void); \
 if (pow(pow(fabs(d1), d2), d3) != pow(fabs(d1),d2*d3) \
     || powf(powf(fabs(f1),f2),f3) != powf(fabs(f1),f2*f3) \
     || powl(powl(fabs(ld1),ld2),ld3) != powl(fabs(ld1),ld2*ld3)) \
    link_failure_pow_pow()

  POW_POW;

  /* Test Nroot(x)*Nroot(y) -> Nroot(x*y).  */
#define ROOT_X_ROOT(FN) \
 extern void link_failure_root_x_root(void); \
 if (FN(d1)*FN(d2) != FN(d1*d2) \
     || FN##f(f1)*FN##f(f2) != FN##f(f1*f2) \
     || FN##l(ld1)*FN##l(ld2) != FN##l(ld1*ld2)) \
    link_failure_root_x_root()

  ROOT_X_ROOT(sqrt);
  ROOT_X_ROOT(cbrt);
  
  /* Test pow(x,y)*pow(x,z) -> pow(x,y+z).  */
#define POW_X_POW \
 extern void link_failure_pow_x_pow(void); \
 if (pow(d1,d2)*pow(d1,d3) != pow(d1,d2+d3) \
     || powf(f1,f2)*powf(f1,f3) != powf(f1,f2+f3) \
     || powl(ld1,ld2)*powl(ld1,ld3) != powl(ld1,ld2+ld3)) \
    link_failure_pow_x_pow()

  POW_X_POW;
  
}

int main (void)
{
  return 0;
}
