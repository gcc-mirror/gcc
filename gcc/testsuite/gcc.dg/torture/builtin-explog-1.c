/* Copyright (C) 2003, 2004  Free Software Foundation.

   Verify that built-in math function constant folding of log & exp is
   correctly performed by the compiler.

   Written by Kaveh Ghazi, 2003-09-05.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

/* Define "e" with as many bits as found in builtins.c:dconste.  */
#define M_E  2.7182818284590452353602874713526624977572470936999595749669676277241
#define M_EF 2.7182818284590452353602874713526624977572470936999595749669676277241F
#define M_EL 2.7182818284590452353602874713526624977572470936999595749669676277241L
/* Precision for comparison tests.  */
#define PREC  (sizeof (float) < sizeof (double) ? 0.0000001 : PRECF)
#define PRECF 0.0001F
#define PRECL (sizeof (float) < sizeof (long double)	\
	       ? 0.0000000000001L : PRECF)
#define PROTOTYPE(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);
#define PROTOTYPE2(FN) extern double FN(double, double); \
  extern float FN##f(float, float); \
  extern long double FN##l(long double, long double);

PROTOTYPE(exp)
PROTOTYPE(exp2)
PROTOTYPE(exp10)
PROTOTYPE(log)
PROTOTYPE(log2)
PROTOTYPE(log10)
PROTOTYPE(pow10)
PROTOTYPE(sqrt)
PROTOTYPE(cbrt)
PROTOTYPE2(pow)

void test(double d1, double d2, float f1, float f2,
	  long double ld1, long double ld2)
{
  /* Test logN(1) -> 0.  */
#define LOG_1(LOG) \
 extern void link_failure_##LOG##_1(void); \
 if (LOG(1.0) != 0.0 || LOG##f(1.0F) != 0.0F || LOG##l(1.0L) != 0.0L) \
    link_failure_##LOG##_1()

  LOG_1(log);
  LOG_1(log2);
  LOG_1(log10);
  
  /* Test logN(N) -> 1.  */
#define LOG_N(LOG, BASE) \
 extern void link_failure_##LOG##_N(void); \
 if (LOG(BASE) != 1.0 || LOG##f(BASE##F) != 1.0F || LOG##l(BASE##L) != 1.0L) \
    link_failure_##LOG##_N()

  LOG_N(log, M_E);
  LOG_N(log2, 2.0);
  LOG_N(log10, 10.0);

  /* Test logN(expN(x)) -> x.  */
#define LOGEXP_SAME(LOG, EXP) \
 extern void link_failure_##LOG##_##EXP##_same(void); \
 if (LOG(EXP(d1)) != d1 || LOG##f(EXP##f(f1)) != f1 \
  || LOG##l(EXP##l(ld1)) != ld1) link_failure_##LOG##_##EXP##_same()

  LOGEXP_SAME(log,exp);
  LOGEXP_SAME(log2,exp2);
  LOGEXP_SAME(log10,exp10);
  LOGEXP_SAME(log10,pow10);

  /* Test logN(expM(x)) -> x*logN(M).  */
#define LOGEXP(LOG, EXP, BASE) \
 extern void link_failure_##LOG##_##EXP(void); \
 if (LOG(EXP(d1)) != d1*LOG(BASE) || LOG##f(EXP##f(f1)) != f1*LOG##f(BASE##F) \
  || LOG##l(EXP##l(ld1)) != ld1*LOG##l(BASE##L)) link_failure_##LOG##_##EXP()

  LOGEXP(log,exp,M_E);
  LOGEXP(log,exp2,2.0);
  LOGEXP(log,exp10,10.0);
  LOGEXP(log,pow10,10.0);
  LOGEXP(log2,exp,M_E);
  LOGEXP(log2,exp2,2.0);
  LOGEXP(log2,exp10,10.0);
  LOGEXP(log2,pow10,10.0);
  LOGEXP(log10,exp,M_E);
  LOGEXP(log10,exp2,2.0);
  LOGEXP(log10,exp10,10.0);
  LOGEXP(log10,pow10,10.0);
  
  /* Test logN(sqrt(x)) -> 0.5*logN(x).  */
#define LOG_SQRT(LOG) \
 extern void link_failure_##LOG##_sqrt(void); \
 if (LOG(sqrt(d1)) != 0.5*LOG(d1) || LOG##f(sqrtf(f1)) != 0.5F*LOG##f(f1) \
  || LOG##l(sqrtl(ld1)) != 0.5L*LOG##l(ld1)) link_failure_##LOG##_sqrt()
    
  LOG_SQRT(log);
  LOG_SQRT(log2);
  LOG_SQRT(log10);
  
  /* Test sqrt(expN(x)) -> expN(x*0.5).  */
#define SQRT_EXP(EXP) \
 extern void link_failure_sqrt_##EXP(void); \
 if (sqrt(EXP(d1)) != EXP(d1*0.5) || sqrtf(EXP##f(f1)) != EXP##f(f1*0.5F) \
  || sqrtl(EXP##l(ld1)) != EXP##l(ld1*0.5L)) link_failure_sqrt_##EXP()
    
  SQRT_EXP(exp);
  SQRT_EXP(exp2);
  SQRT_EXP(exp10);
  SQRT_EXP(pow10);
  
  /* Test logN(cbrt(x)) -> (1/3)*logN(x).  */
#define LOG_CBRT(LOG) \
 extern void link_failure_##LOG##_cbrt(void); \
 if (LOG(cbrt(d1)) != (1.0/3)*LOG(d1) \
  || LOG##f(cbrtf(f1)) != (1.0F/3)*LOG##f(f1) \
  || LOG##l(cbrtl(ld1)) != (1.0L/3)*LOG##l(ld1)) link_failure_##LOG##_cbrt()
    
  LOG_CBRT(log);
  LOG_CBRT(log2);
  LOG_CBRT(log10);
  
  /* Test cbrt(expN(x)) -> expN(x/3).  */
#define CBRT_EXP(EXP) \
 extern void link_failure_cbrt_##EXP(void); \
 if (cbrt(EXP(d1)) != EXP(d1/3.0) || cbrtf(EXP##f(f1)) != EXP##f(f1/3.0F) \
  || cbrtl(EXP##l(ld1)) != EXP##l(ld1/3.0L)) link_failure_cbrt_##EXP()
    
  CBRT_EXP(exp);
  CBRT_EXP(exp2);
  CBRT_EXP(exp10);
  CBRT_EXP(pow10);
  
  /* Test logN(pow(x,y)) -> y*logN(x).  */
#define LOG_POW(LOG, POW) \
 extern void link_failure_##LOG##_##POW(void); \
 if (LOG(POW(d1,d2)) != d2*LOG(d1) || LOG##f(POW##f(f1,f2)) != f2*LOG##f(f1) \
  || LOG##l(POW##l(ld1,ld2)) != ld2*LOG##l(ld1)) link_failure_##LOG##_##POW()
  
  LOG_POW(log,pow);
  LOG_POW(log2,pow);
  LOG_POW(log10,pow);

  /* Test pow(expN(x),y)) -> expN(x*y).  */
#define POW_EXP(POW, EXP) \
 extern void link_failure_##POW##_##EXP(void); \
 if (POW(EXP(d1),d2) != EXP(d1*d2) || POW##f(EXP##f(f1),f2) != EXP##f(f1*f2) \
  || POW##l(EXP##l(ld1),ld2) != EXP##l(ld1*ld2)) link_failure_##POW##_##EXP()
  
  POW_EXP(pow, exp);
  POW_EXP(pow, exp2);
  POW_EXP(pow, exp10);
  POW_EXP(pow, pow10);

  /* Test expN(0) -> 1.  */
#define EXP_0(EXP) \
 extern void link_failure_##EXP##_0(void); \
 if (EXP(0.0) != 1.0 || EXP##f(0.0F) != 1.0F || EXP##l(0.0L) != 1.0L) \
  link_failure_##EXP##_0()

  EXP_0(exp);
  EXP_0(exp2);
  EXP_0(exp10);
  EXP_0(pow10);
  
  /* Test expN(1) -> N.  */
#define EXP_N(EXP, BASE) \
 extern void link_failure_##EXP##_N(void); \
 if (EXP(1.0) != BASE || EXP##f(1.0F) != BASE##F || EXP##l(1.0L) != BASE##L) \
  link_failure_##EXP##_N()

  EXP_N(exp, M_E);
  EXP_N(exp2, 2.0);
  EXP_N(exp10, 10.0);
  EXP_N(pow10, 10.0);

  /* Test expN(integer) -> N*N*N*...  */
#define EXP_INT(EXP, BASE) \
 extern void link_failure_##EXP##_INT(void); \
 if (EXP(5.0) < (BASE)*(BASE)*(BASE)*(BASE)*(BASE) - PREC \
  || EXP(5.0) > (BASE)*(BASE)*(BASE)*(BASE)*(BASE) + PREC \
  || EXP##f(5.0F) < (BASE##F)*(BASE##F)*(BASE##F)*(BASE##F)*(BASE##F) -PRECF \
  || EXP##f(5.0F) > (BASE##F)*(BASE##F)*(BASE##F)*(BASE##F)*(BASE##F) +PRECF \
  || EXP##l(5.0L) < (BASE##L)*(BASE##L)*(BASE##L)*(BASE##L)*(BASE##L) -PRECL \
  || EXP##l(5.0L) > (BASE##L)*(BASE##L)*(BASE##L)*(BASE##L)*(BASE##L) +PRECL) \
   link_failure_##EXP##_INT()

  EXP_INT(exp, M_E);
  EXP_INT(exp2, 2.0);
  EXP_INT(exp10, 10.0);
  EXP_INT(pow10, 10.0);

  /* Test expN(logN(x)) -> x.  */
#define EXPLOG_SAME(EXP, LOG) \
 extern void link_failure_##EXP##_##LOG##_same(void); \
 if (EXP(LOG(d1)) != d1 || EXP##f(LOG##f(f1)) != f1 \
  || EXP##l(LOG##l(ld1)) != ld1) link_failure_##EXP##_##LOG##_same()

  EXPLOG_SAME(exp, log);
  EXPLOG_SAME(exp2, log2);
  EXPLOG_SAME(exp10, log10);
  EXPLOG_SAME(pow10, log10);

  /* Test expN(x)*expN(y)) -> expN(x+y).  */
#define EXPXEXP(EXP) \
 extern void link_failure_##EXP##X##EXP(void); \
 if (EXP(d1)*EXP(d2) != EXP(d1+d2) || EXP##f(f1)*EXP##f(f2) != EXP##f(f1+f2) \
  || EXP##l(ld1)*EXP##l(ld2) != EXP##l(ld1+ld2)) link_failure_##EXP##X##EXP()

  EXPXEXP(exp);
  EXPXEXP(exp2);
  EXPXEXP(exp10);
  EXPXEXP(pow10);

  /* Test x/expN(y) -> x*expN(-y).  */
  /* Test expN(x)/expN(y) -> expN(x-y).  */
#define DIVEXP(EXP) \
 extern void link_failure_div1_##EXP(void); \
 if (d1/EXP(d2) != d1*EXP(-d2) || f1/EXP##f(f2) != f1*EXP##f(-f2) \
  || ld1/EXP##l(ld2) != ld1*EXP##l(-ld2)) link_failure_div1_##EXP(); \
 extern void link_failure_div2_##EXP(void); \
 if (EXP(d1)/EXP(d2) != EXP(d1-d2) || EXP##f(f1)/EXP##f(f2) != EXP##f(f1-f2) \
  || EXP##l(ld1)/EXP##l(ld2) != EXP##l(ld1-ld2)) link_failure_div2_##EXP()

  DIVEXP(exp);
  DIVEXP(exp2);
  DIVEXP(exp10);
  DIVEXP(pow10);
}

int main (void)
{
  return 0;
}
