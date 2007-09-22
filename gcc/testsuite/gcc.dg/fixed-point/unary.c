/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.3 Unary operators.

   Check unary operators ++ -- + - !.  */

extern void abort (void);

#define INCDEC(TYPE,POSTFIX) \
  { TYPE a = 0.0 ## POSTFIX; \
    if (a++ != 0.0 ## POSTFIX) \
      abort(); \
    if (a != 1.0 ## POSTFIX) \
      abort(); \
    if (a-- != 1.0 ## POSTFIX) \
      abort(); \
    if (a != 0.0 ## POSTFIX) \
      abort(); \
    if (++a != 1.0 ## POSTFIX) \
      abort(); \
    if (a != 1.0 ## POSTFIX) \
      abort(); \
    if (--a != 0.0 ## POSTFIX) \
      abort(); \
    if (a != 0.0 ## POSTFIX) \
      abort(); \
  }

#define FRACT_INCDEC(TYPE,POSTFIX) \
  { TYPE a = -0.5 ## POSTFIX; \
    if (a++ != -0.5 ## POSTFIX) \
      abort(); \
    if (a != 0.5 ## POSTFIX) \
      abort(); \
    if (a-- != 0.5 ## POSTFIX) \
      abort(); \
    if (a != -0.5 ## POSTFIX) \
      abort(); \
    if (++a != 0.5 ## POSTFIX) \
      abort(); \
    if (a != 0.5 ## POSTFIX) \
      abort(); \
    if (--a != -0.5 ## POSTFIX) \
      abort(); \
    if (a != -0.5 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_INCDEC(TYPE,POSTFIX) \
  { TYPE a = 0.9 ## POSTFIX; \
    if (++a != 1.0 ## POSTFIX) \
      abort(); \
    a = -0.1 ## POSTFIX; \
    if (--a != -0.5 ## POSTFIX - 0.5 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_UNS_INCDEC(TYPE,POSTFIX) \
  { TYPE a = 0.9 ## POSTFIX; \
    if (++a != 1.0 ## POSTFIX) \
      abort(); \
    a = 0.1 ## POSTFIX; \
    if (--a != 0.0 ## POSTFIX) \
      abort(); \
  }

#define PLUS(TYPE,POSTFIX) \
  { TYPE a = 0.1 ## POSTFIX; \
    if (+a != 0.1 ## POSTFIX) \
      abort(); \
  }

#define NEG(TYPE,POSTFIX) \
  { TYPE a = -0.1 ## POSTFIX; \
    if (-a != 0.1 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_NEG(TYPE,POSTFIX) \
  { TYPE a = -0.5 ## POSTFIX - 0.5 ## POSTFIX; \
    if (-a != 1.0 ## POSTFIX) \
      abort(); \
  }

#define UNS_NEG(TYPE,POSTFIX) \
  { TYPE a = 0.0 ## POSTFIX; \
    if (-a != 0.0 ## POSTFIX) \
      abort(); \
  }

#define FRACT_SAT_UNS_NEG(TYPE,POSTFIX) \
  { TYPE a = 0.5 ## POSTFIX; \
    if (-a != 0.0 ## POSTFIX) \
      abort(); \
  }

#define LOGNEG(TYPE,POSTFIX) \
  { TYPE a = 0.0 ## POSTFIX; \
    TYPE b = 0.1 ## POSTFIX; \
    if (!a != 1) \
      abort(); \
    if (!b != 0) \
      abort(); \
  }

int main ()
{
  FRACT_INCDEC(short _Fract, hr);
  FRACT_INCDEC(_Fract, r);
  FRACT_INCDEC(long _Fract, lr);
  FRACT_INCDEC(long long _Fract, llr);
  FRACT_INCDEC(_Sat short _Fract, hr);
  FRACT_INCDEC(_Sat _Fract, r);
  FRACT_INCDEC(_Sat long _Fract, lr);
  FRACT_INCDEC(_Sat long long _Fract, llr);

  INCDEC(short _Accum, hk);
  INCDEC(_Accum, k);
  INCDEC(long _Accum, lk);
  INCDEC(long long _Accum, llk);
  INCDEC(unsigned short _Accum, uhk);
  INCDEC(unsigned _Accum, uk);
  INCDEC(unsigned long _Accum, ulk);
  INCDEC(unsigned long long _Accum, ullk);
  INCDEC(_Sat short _Accum, hk);
  INCDEC(_Sat _Accum, k);
  INCDEC(_Sat long _Accum, lk);
  INCDEC(_Sat long long _Accum, llk);
  INCDEC(_Sat unsigned short _Accum, uhk);
  INCDEC(_Sat unsigned _Accum, uk);
  INCDEC(_Sat unsigned long _Accum, ulk);
  INCDEC(_Sat unsigned long long _Accum, ullk);

  FRACT_SAT_INCDEC(_Sat short _Fract, hr);
  FRACT_SAT_INCDEC(_Sat _Fract, r);
  FRACT_SAT_INCDEC(_Sat long _Fract, lr);
  FRACT_SAT_INCDEC(_Sat long long _Fract, llr);

  FRACT_SAT_UNS_INCDEC(_Sat unsigned short _Fract, uhr);
  FRACT_SAT_UNS_INCDEC(_Sat unsigned _Fract, ur);
  FRACT_SAT_UNS_INCDEC(_Sat unsigned long _Fract, ulr);
  FRACT_SAT_UNS_INCDEC(_Sat unsigned long long _Fract, ullr);

  PLUS(short _Fract, hr);
  PLUS(_Fract, r);
  PLUS(long _Fract, lr);
  PLUS(long long _Fract, llr);
  PLUS(unsigned short _Fract, uhr);
  PLUS(unsigned _Fract, ur);
  PLUS(unsigned long _Fract, ulr);
  PLUS(unsigned long long _Fract, ullr);
  PLUS(_Sat short _Fract, hr);
  PLUS(_Sat _Fract, r);
  PLUS(_Sat long _Fract, lr);
  PLUS(_Sat long long _Fract, llr);
  PLUS(_Sat unsigned short _Fract, uhr);
  PLUS(_Sat unsigned _Fract, ur);
  PLUS(_Sat unsigned long _Fract, ulr);
  PLUS(_Sat unsigned long long _Fract, ullr);
  PLUS(short _Accum, hk);
  PLUS(_Accum, k);
  PLUS(long _Accum, lk);
  PLUS(long long _Accum, llk);
  PLUS(unsigned short _Accum, uhk);
  PLUS(unsigned _Accum, uk);
  PLUS(unsigned long _Accum, ulk);
  PLUS(unsigned long long _Accum, ullk);
  PLUS(_Sat short _Accum, hk);
  PLUS(_Sat _Accum, k);
  PLUS(_Sat long _Accum, lk);
  PLUS(_Sat long long _Accum, llk);
  PLUS(_Sat unsigned short _Accum, uhk);
  PLUS(_Sat unsigned _Accum, uk);
  PLUS(_Sat unsigned long _Accum, ulk);
  PLUS(_Sat unsigned long long _Accum, ullk);

  NEG(short _Fract, hr);
  NEG(_Fract, r);
  NEG(long _Fract, lr);
  NEG(long long _Fract, llr);
  NEG(_Sat short _Fract, hr);
  NEG(_Sat _Fract, r);
  NEG(_Sat long _Fract, lr);
  NEG(_Sat long long _Fract, llr);
  NEG(short _Accum, hk);
  NEG(_Accum, k);
  NEG(long _Accum, lk);
  NEG(long long _Accum, llk);
  NEG(_Sat short _Accum, hk);
  NEG(_Sat _Accum, k);
  NEG(_Sat long _Accum, lk);
  NEG(_Sat long long _Accum, llk);

  FRACT_SAT_NEG(_Sat short _Fract, hr);
  FRACT_SAT_NEG(_Sat _Fract, r);
  FRACT_SAT_NEG(_Sat long _Fract, lr);
  FRACT_SAT_NEG(_Sat long long _Fract, llr);

  UNS_NEG(short _Fract, hr);
  UNS_NEG(_Fract, r);
  UNS_NEG(long _Fract, lr);
  UNS_NEG(long long _Fract, llr);
  UNS_NEG(_Sat short _Fract, hr);
  UNS_NEG(_Sat _Fract, r);
  UNS_NEG(_Sat long _Fract, lr);
  UNS_NEG(_Sat long long _Fract, llr);
  UNS_NEG(short _Accum, hk);
  UNS_NEG(_Accum, k);
  UNS_NEG(long _Accum, lk);
  UNS_NEG(long long _Accum, llk);
  UNS_NEG(_Sat short _Accum, hk);
  UNS_NEG(_Sat _Accum, k);
  UNS_NEG(_Sat long _Accum, lk);
  UNS_NEG(_Sat long long _Accum, llk);

  FRACT_SAT_UNS_NEG(_Sat unsigned short _Fract, uhr);
  FRACT_SAT_UNS_NEG(_Sat unsigned _Fract, ur);
  FRACT_SAT_UNS_NEG(_Sat unsigned long _Fract, ulr);
  FRACT_SAT_UNS_NEG(_Sat unsigned long long _Fract, ullr);

  LOGNEG(short _Fract, hr);
  LOGNEG(_Fract, r);
  LOGNEG(long _Fract, lr);
  LOGNEG(long long _Fract, llr);
  LOGNEG(unsigned short _Fract, uhr);
  LOGNEG(unsigned _Fract, ur);
  LOGNEG(unsigned long _Fract, ulr);
  LOGNEG(unsigned long long _Fract, ullr);
  LOGNEG(_Sat short _Fract, hr);
  LOGNEG(_Sat _Fract, r);
  LOGNEG(_Sat long _Fract, lr);
  LOGNEG(_Sat long long _Fract, llr);
  LOGNEG(_Sat unsigned short _Fract, uhr);
  LOGNEG(_Sat unsigned _Fract, ur);
  LOGNEG(_Sat unsigned long _Fract, ulr);
  LOGNEG(_Sat unsigned long long _Fract, ullr);
  LOGNEG(short _Accum, hk);
  LOGNEG(_Accum, k);
  LOGNEG(long _Accum, lk);
  LOGNEG(long long _Accum, llk);
  LOGNEG(unsigned short _Accum, uhk);
  LOGNEG(unsigned _Accum, uk);
  LOGNEG(unsigned long _Accum, ulk);
  LOGNEG(unsigned long long _Accum, ullk);
  LOGNEG(_Sat short _Accum, hk);
  LOGNEG(_Sat _Accum, k);
  LOGNEG(_Sat long _Accum, lk);
  LOGNEG(_Sat long long _Accum, llk);
  LOGNEG(_Sat unsigned short _Accum, uhk);
  LOGNEG(_Sat unsigned _Accum, uk);
  LOGNEG(_Sat unsigned long _Accum, ulk);
  LOGNEG(_Sat unsigned long long _Accum, ullk);

  return 0;
}
