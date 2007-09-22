/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.5.7 - Bitwise shift operands.
   C99 6.5.5 Multiplicative operators.
   C99 6.5.6 Additive operators.
   C99 6.5.7 Bitwise shift operators.
   C99 6.5.8 Relational operators.
   C99 6.5.9 Equality operators.
   C99 6.5.16 Assignment operators.

   Check binary operators + - * / << >> < <= >= > == != += -= *= /= <<= >>=.  */

extern void abort (void);

#define BINARY(TYPE,POSTFIX) \
  { TYPE a = 0.5 ## POSTFIX; \
    TYPE b = 0.25 ## POSTFIX; \
    if (a + b != 0.75 ## POSTFIX) \
      abort(); \
    if (a - b != 0.25 ## POSTFIX) \
      abort(); \
    if (a * b != 0.125 ## POSTFIX) \
      abort(); \
    if (b / a != 0.5 ## POSTFIX) \
      abort(); \
    if (b << 1 != a) \
      abort(); \
    if (a >> 1 != b) \
      abort(); \
    if (a < b != 0) \
      abort(); \
    if (a <= b != 0) \
      abort(); \
    if (a > b != 1) \
      abort(); \
    if (a >= b != 1) \
      abort(); \
    if (a == b != 0) \
      abort(); \
    if (a != b != 1) \
      abort(); \
    a += b; \
    if (a != 0.75 ## POSTFIX) \
      abort(); \
    a -= b; \
    if (a != 0.5 ## POSTFIX) \
      abort(); \
    a *= b; \
    if (a != 0.125 ## POSTFIX) \
      abort(); \
    a /= b; \
    if (a != 0.5 ## POSTFIX) \
      abort(); \
    a >>= 2; \
    if (a != 0.125 ## POSTFIX) \
      abort(); \
    a <<= 1; \
    if (a != 0.25 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_BINARY(TYPE,POSTFIX) \
  { TYPE a = 0.7 ## POSTFIX; \
    TYPE b = 0.9 ## POSTFIX; \
    if (a + b != 1.0 ## POSTFIX) \
      abort(); \
    a = -0.7 ## POSTFIX; \
    b = -0.9 ## POSTFIX; \
    if (a + b != -0.5 ## POSTFIX - 0.5 ## POSTFIX) \
      abort(); \
    a = 0.7 ## POSTFIX; \
    b = -0.9 ## POSTFIX; \
    if (a - b != 1.0 ## POSTFIX) \
      abort(); \
    a = -0.7 ## POSTFIX; \
    b = 0.9 ## POSTFIX; \
    if (a - b != -0.5 ## POSTFIX - 0.5 ## POSTFIX) \
      abort(); \
    a = -0.5 ## POSTFIX - 0.5 ## POSTFIX; \
    if (a * a != 1.0 ## POSTFIX) \
      abort(); \
    a = 0.8 ## POSTFIX; \
    b = 0.5 ## POSTFIX; \
    if (a / b != 1.0 ## POSTFIX) \
      abort(); \
    a = -0.8 ## POSTFIX; \
    b = 0.5 ## POSTFIX; \
    if (a / b != -0.5 ## POSTFIX - 0.5 ## POSTFIX) \
      abort(); \
    a = 0.1 ## POSTFIX; \
    if (a << 4 != 1.0 ## POSTFIX) \
      abort(); \
    a = -0.8 ## POSTFIX; \
    if (a << 4 != -0.5 ## POSTFIX - 0.5 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_UNS_BINARY(TYPE,POSTFIX) \
  { TYPE a = 0.7 ## POSTFIX; \
    TYPE b = 0.9 ## POSTFIX; \
    if (a + b != 1.0 ## POSTFIX) \
      abort(); \
    if (a - b != 0.0 ## POSTFIX) \
      abort(); \
    if (b / a != 1.0 ## POSTFIX) \
      abort(); \
    if (a << 1 != 1.0 ## POSTFIX) \
      abort(); \
  }

int main ()
{
  BINARY(short _Fract, hr);
  BINARY(_Fract, r);
  BINARY(long _Fract, lr);
  BINARY(long long _Fract, llr);
  BINARY(unsigned short _Fract, uhr);
  BINARY(unsigned _Fract, ur);
  BINARY(unsigned long _Fract, ulr);
  BINARY(unsigned long long _Fract, ullr);
  BINARY(_Sat short _Fract, hr);
  BINARY(_Sat _Fract, r);
  BINARY(_Sat long _Fract, lr);
  BINARY(_Sat long long _Fract, llr);
  BINARY(_Sat unsigned short _Fract, uhr);
  BINARY(_Sat unsigned _Fract, ur);
  BINARY(_Sat unsigned long _Fract, ulr);
  BINARY(_Sat unsigned long long _Fract, ullr);
  BINARY(short _Accum, hk);
  BINARY(_Accum, k);
  BINARY(long _Accum, lk);
  BINARY(long long _Accum, llk);
  BINARY(unsigned short _Accum, uhk);
  BINARY(unsigned _Accum, uk);
  BINARY(unsigned long _Accum, ulk);
  BINARY(unsigned long long _Accum, ullk);
  BINARY(_Sat short _Accum, hk);
  BINARY(_Sat _Accum, k);
  BINARY(_Sat long _Accum, lk);
  BINARY(_Sat long long _Accum, llk);
  BINARY(_Sat unsigned short _Accum, uhk);
  BINARY(_Sat unsigned _Accum, uk);
  BINARY(_Sat unsigned long _Accum, ulk);
  BINARY(_Sat unsigned long long _Accum, ullk);

  FRACT_SAT_BINARY(_Sat short _Fract, hr);
  FRACT_SAT_BINARY(_Sat _Fract, r);
  FRACT_SAT_BINARY(_Sat long _Fract, lr);
  FRACT_SAT_BINARY(_Sat long long _Fract, llr);

  FRACT_SAT_UNS_BINARY(_Sat unsigned short _Fract, uhr);
  FRACT_SAT_UNS_BINARY(_Sat unsigned _Fract, ur);
  FRACT_SAT_UNS_BINARY(_Sat unsigned long _Fract, ulr);
  FRACT_SAT_UNS_BINARY(_Sat unsigned long long _Fract, ullr);

  return 0;
}
