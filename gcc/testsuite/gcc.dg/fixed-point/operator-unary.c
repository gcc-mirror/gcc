/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.3 Unary operators & and *.
   Based on the test from ../dfp/.  */

extern void abort (void);

#define UNARY_OPERATOR(TYPE,SUFFIX)	\
do					\
{					\
 TYPE unary_d = 0.1 ## SUFFIX;		\
 TYPE* unary_dp;			\
 /*  & operator.  */			\
 unary_dp = &(unary_d);			\
 /*  * operator.  */			\
 unary_d = *(unary_dp);			\
} while (0)

int
main ()
{
  /*  C99 6.5.3 Unary operators.  */
  UNARY_OPERATOR(short _Fract, hr);
  UNARY_OPERATOR(_Fract, r);
  UNARY_OPERATOR(long _Fract, lr);
  UNARY_OPERATOR(long long _Fract, llr);
  UNARY_OPERATOR(unsigned short _Fract, uhr);
  UNARY_OPERATOR(unsigned _Fract, ur);
  UNARY_OPERATOR(unsigned long _Fract, ulr);
  UNARY_OPERATOR(unsigned long long _Fract, ullr);
  UNARY_OPERATOR(_Sat short _Fract, hr);
  UNARY_OPERATOR(_Sat _Fract, r);
  UNARY_OPERATOR(_Sat long _Fract, lr);
  UNARY_OPERATOR(_Sat long long _Fract, llr);
  UNARY_OPERATOR(_Sat unsigned short _Fract, uhr);
  UNARY_OPERATOR(_Sat unsigned _Fract, ur);
  UNARY_OPERATOR(_Sat unsigned long _Fract, ulr);
  UNARY_OPERATOR(_Sat unsigned long long _Fract, ullr);
  UNARY_OPERATOR(short _Accum, hk);
  UNARY_OPERATOR(_Accum, k);
  UNARY_OPERATOR(long _Accum, lk);
  UNARY_OPERATOR(long long _Accum, llk);
  UNARY_OPERATOR(unsigned short _Accum, uhk);
  UNARY_OPERATOR(unsigned _Accum, uk);
  UNARY_OPERATOR(unsigned long _Accum, ulk);
  UNARY_OPERATOR(unsigned long long _Accum, ullk);
  UNARY_OPERATOR(_Sat short _Accum, hk);
  UNARY_OPERATOR(_Sat _Accum, k);
  UNARY_OPERATOR(_Sat long _Accum, lk);
  UNARY_OPERATOR(_Sat long long _Accum, llk);
  UNARY_OPERATOR(_Sat unsigned short _Accum, uhk);
  UNARY_OPERATOR(_Sat unsigned _Accum, uk);
  UNARY_OPERATOR(_Sat unsigned long _Accum, ulk);
  UNARY_OPERATOR(_Sat unsigned long long _Accum, ullk);

  return 0;
}
