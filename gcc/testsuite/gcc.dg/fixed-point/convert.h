/* Check conversions involving fixed-point.
 * Break up use-site into into manageable parts so that even embedded
 * targets with restrictive resources can run them.  */

/* Fixed-point to fixed-point.  */
#define CONV(TYPE1,POSTFIX1,TYPE2,POSTFIX2) \
  { \
    TYPE1 a = 0.5 ## POSTFIX1; \
    TYPE2 b = a; \
    if (b != 0.5 ## POSTFIX2) \
      abort(); \
  }

/* TYPE1 with VALUE1 to TYPE2 with VALUE2.  */
#define CONV2(TYPE1,VALUE1,TYPE2,VALUE2) \
  { \
    TYPE1 a = VALUE1; \
    TYPE2 b = a; \
    if (b != VALUE2) \
      abort(); \
  }

/* Fixed-point to integer, and integer to fixed-point.  */
#define CONV_INT(TYPE1,POSTFIX1,TYPE2) \
  { \
    TYPE1 a = 0.5 ## POSTFIX1; \
    TYPE2 b = a; \
    TYPE2 c = 0; \
    TYPE1 d = c; \
    if (b != 0) \
      abort(); \
    if (d != 0.0 ## POSTFIX1) \
      abort(); \
  }

/* Signed fixed-point to integer.  */
#define CONV_INT2(TYPE1,POSTFIX1,TYPE2) \
  { \
    TYPE1 a = -0.5 ## POSTFIX1; \
    TYPE2 b = a; \
    if (b != 0) \
      abort(); \
    a = -0.0 ## POSTFIX1; \
    b = a; \
    if (b != 0) \
      abort(); \
    a = +0.0 ## POSTFIX1; \
    b = a; \
    if (b != 0) \
      abort(); \
    a = +0.1 ## POSTFIX1; \
    b = a; \
    if (b != 0) \
      abort(); \
    a = -0.1 ## POSTFIX1; \
    b = a; \
    if (b != 0) \
      abort(); \
  }

/* Signed fixed-point to signed integer.  */
#define CONV_INT3(TYPE1,POSTFIX1,TYPE2) \
  { \
    TYPE1 a = -0.5 ## POSTFIX1 - 0.5 ## POSTFIX1; \
    TYPE2 b = a; \
    if (b != -1) \
      abort(); \
  }

/* Fixed-point to floating-point, and floating-point to fixed-point.  */
#define CONV_FLOAT(TYPE1,POSTFIX1,TYPE2) \
  { \
    TYPE1 a = 0.5 ## POSTFIX1; \
    TYPE2 b = a; \
    TYPE2 c = 0.25; \
    TYPE1 d = c; \
    if (b != 0.5) \
      abort(); \
    if (d != 0.25 ## POSTFIX1) \
      abort(); \
  }

/* Accum to integer, and integer to accum.  */
#define CONV_ACCUM_INT(TYPE1,POSTFIX1,TYPE2) \
  { \
    TYPE1 a = 99.12345 ## POSTFIX1; \
    TYPE2 b = a; \
    TYPE2 c = 123; \
    TYPE1 d = c; \
    if (b != 99) \
      abort(); \
    if (d != 123.0 ## POSTFIX1) \
      abort(); \
  }

#define ALL_CONV(TYPE,POSTFIX) \
  CONV(TYPE, POSTFIX, short _Fract, hr) \
  CONV(TYPE, POSTFIX, _Fract, r) \
  CONV(TYPE, POSTFIX, long _Fract, lr) \
  CONV(TYPE, POSTFIX, long long _Fract, llr) \
  CONV(TYPE, POSTFIX, unsigned short _Fract, uhr) \
  CONV(TYPE, POSTFIX, unsigned _Fract, ur) \
  CONV(TYPE, POSTFIX, unsigned long _Fract, ulr) \
  CONV(TYPE, POSTFIX, unsigned long long _Fract, ullr) \
  CONV(TYPE, POSTFIX, short _Accum, hk) \
  CONV(TYPE, POSTFIX, _Accum, k) \
  CONV(TYPE, POSTFIX, long _Accum, lk) \
  CONV(TYPE, POSTFIX, long long _Accum, llk) \
  CONV(TYPE, POSTFIX, unsigned short _Accum, uhk) \
  CONV(TYPE, POSTFIX, unsigned _Accum, uk) \
  CONV(TYPE, POSTFIX, unsigned long _Accum, ulk) \
  CONV(TYPE, POSTFIX, unsigned long long _Accum, ullk) \
  CONV(_Sat TYPE, POSTFIX, short _Fract, hr) \
  CONV(_Sat TYPE, POSTFIX, _Fract, r) \
  CONV(_Sat TYPE, POSTFIX, long _Fract, lr) \
  CONV(_Sat TYPE, POSTFIX, long long _Fract, llr) \
  CONV(_Sat TYPE, POSTFIX, unsigned short _Fract, uhr) \
  CONV(_Sat TYPE, POSTFIX, unsigned _Fract, ur) \
  CONV(_Sat TYPE, POSTFIX, unsigned long _Fract, ulr) \
  CONV(_Sat TYPE, POSTFIX, unsigned long long _Fract, ullr) \
  CONV(_Sat TYPE, POSTFIX, short _Accum, hk) \
  CONV(_Sat TYPE, POSTFIX, _Accum, k) \
  CONV(_Sat TYPE, POSTFIX, long _Accum, lk) \
  CONV(_Sat TYPE, POSTFIX, long long _Accum, llk) \
  CONV(_Sat TYPE, POSTFIX, unsigned short _Accum, uhk) \
  CONV(_Sat TYPE, POSTFIX, unsigned _Accum, uk) \
  CONV(_Sat TYPE, POSTFIX, unsigned long _Accum, ulk) \
  CONV(_Sat TYPE, POSTFIX, unsigned long long _Accum, ullk) \
  CONV(TYPE, POSTFIX, _Sat short _Fract, hr)                \
  CONV(TYPE, POSTFIX, _Sat _Fract, r) \
  CONV(TYPE, POSTFIX, _Sat long _Fract, lr) \
  CONV(TYPE, POSTFIX, _Sat long long _Fract, llr) \
  CONV(TYPE, POSTFIX, _Sat unsigned short _Fract, uhr) \
  CONV(TYPE, POSTFIX, _Sat unsigned _Fract, ur) \
  CONV(TYPE, POSTFIX, _Sat unsigned long _Fract, ulr) \
  CONV(TYPE, POSTFIX, _Sat unsigned long long _Fract, ullr) \
  CONV(TYPE, POSTFIX, _Sat short _Accum, hk) \
  CONV(TYPE, POSTFIX, _Sat _Accum, k) \
  CONV(TYPE, POSTFIX, _Sat long _Accum, lk) \
  CONV(TYPE, POSTFIX, _Sat long long _Accum, llk) \
  CONV(TYPE, POSTFIX, _Sat unsigned short _Accum, uhk) \
  CONV(TYPE, POSTFIX, _Sat unsigned _Accum, uk) \
  CONV(TYPE, POSTFIX, _Sat unsigned long _Accum, ulk) \
  CONV(TYPE, POSTFIX, _Sat unsigned long long _Accum, ullk) \
  CONV_INT(TYPE, POSTFIX, signed char)                      \
  CONV_INT(TYPE, POSTFIX, short) \
  CONV_INT(TYPE, POSTFIX, int) \
  CONV_INT(TYPE, POSTFIX, long) \
  CONV_INT(TYPE, POSTFIX, long long) \
  CONV_INT(TYPE, POSTFIX, unsigned char) \
  CONV_INT(TYPE, POSTFIX, unsigned short) \
  CONV_INT(TYPE, POSTFIX, unsigned int) \
  CONV_INT(TYPE, POSTFIX, unsigned long) \
  CONV_INT(TYPE, POSTFIX, unsigned long long) \
  CONV_INT(_Sat TYPE, POSTFIX, signed char) \
  CONV_INT(_Sat TYPE, POSTFIX, short) \
  CONV_INT(_Sat TYPE, POSTFIX, int) \
  CONV_INT(_Sat TYPE, POSTFIX, long) \
  CONV_INT(_Sat TYPE, POSTFIX, long long) \
  CONV_INT(_Sat TYPE, POSTFIX, unsigned char) \
  CONV_INT(_Sat TYPE, POSTFIX, unsigned short) \
  CONV_INT(_Sat TYPE, POSTFIX, unsigned int) \
  CONV_INT(_Sat TYPE, POSTFIX, unsigned long) \
  CONV_INT(_Sat TYPE, POSTFIX, unsigned long long)

#define ALL_CONV_FLOAT(TYPE,POSTFIX) \
  CONV_FLOAT(TYPE, POSTFIX, float)    \
  CONV_FLOAT(TYPE, POSTFIX, double) \
  CONV_FLOAT(_Sat TYPE, POSTFIX, float) \
  CONV_FLOAT(_Sat TYPE, POSTFIX, double)

#define ALL_ACCUM_CONV(TYPE,POSTFIX) \
  CONV_ACCUM_INT(TYPE, POSTFIX, signed char) \
  CONV_ACCUM_INT(TYPE, POSTFIX, short) \
  CONV_ACCUM_INT(TYPE, POSTFIX, int) \
  CONV_ACCUM_INT(TYPE, POSTFIX, long) \
  CONV_ACCUM_INT(TYPE, POSTFIX, long long) \
  CONV_ACCUM_INT(TYPE, POSTFIX, unsigned char) \
  CONV_ACCUM_INT(TYPE, POSTFIX, unsigned short) \
  CONV_ACCUM_INT(TYPE, POSTFIX, unsigned int) \
  CONV_ACCUM_INT(TYPE, POSTFIX, unsigned long) \
  CONV_ACCUM_INT(TYPE, POSTFIX, unsigned long long) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, signed char) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, short) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, int) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, long) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, long long) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, unsigned char) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, unsigned short) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, unsigned int) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, unsigned long) \
  CONV_ACCUM_INT(_Sat TYPE, POSTFIX, unsigned long long)

#define NEG_CONV(TYPE,POSTFIX) \
  CONV_INT2(TYPE, POSTFIX, signed char) \
  CONV_INT2(TYPE, POSTFIX, short) \
  CONV_INT2(TYPE, POSTFIX, int) \
  CONV_INT2(TYPE, POSTFIX, long) \
  CONV_INT2(TYPE, POSTFIX, long long) \
  CONV_INT2(TYPE, POSTFIX, unsigned char) \
  CONV_INT2(TYPE, POSTFIX, unsigned short) \
  CONV_INT2(TYPE, POSTFIX, unsigned int) \
  CONV_INT2(TYPE, POSTFIX, unsigned long) \
  CONV_INT2(TYPE, POSTFIX, unsigned long long) \
  CONV_INT3(TYPE, POSTFIX, signed char) \
  CONV_INT3(TYPE, POSTFIX, short) \
  CONV_INT3(TYPE, POSTFIX, int) \
  CONV_INT3(TYPE, POSTFIX, long) \
  CONV_INT3(TYPE, POSTFIX, long long)

/* Signed accum to _Sat unsigned/signed fract.  */
#define SAT_CONV1(TYPE, POSTFIX) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -3.2 ## POSTFIX, _Sat unsigned long long _Fract, 0.0ullr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -1.0 ## POSTFIX, _Sat unsigned long long _Fract, 0.0ullr)

/* Unsigned accum to _Sat unsigned/signed fract.  */
#define SAT_CONV2(TYPE, POSTFIX) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat _Fract, 1.0r) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, 3.2 ## POSTFIX, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat _Fract, 1.0r) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, 1.0 ## POSTFIX, _Sat unsigned long long _Fract, 1.0ullr)

/* Signed fract to _Sat unsigned fract.  */
#define SAT_CONV3(TYPE, POSTFIX) \
  CONV2(TYPE, -0.5 ## POSTFIX, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, -0.5 ## POSTFIX, _Sat unsigned  _Fract, 0.0ur) \
  CONV2(TYPE, -0.5 ## POSTFIX, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, -0.5 ## POSTFIX, _Sat unsigned long long _Fract, 0.0ullr)

/* Signed integer to _Sat signed/unsigned fract.  */
#define SAT_CONV4(TYPE) \
  CONV2(TYPE, 100, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -100, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 100, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -100, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 100, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -100, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 100, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -100, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 100, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -100, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 100, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -100, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 100, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -100, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 100, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -100, _Sat unsigned long long _Fract, 0.0ullr) \
  CONV2(TYPE, 1, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -1, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 1, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -1, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 1, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -1, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 1, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -1, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 1, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -1, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 1, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -1, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 1, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -1, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 1, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -1, _Sat unsigned long long _Fract, 0.0ullr)

/* Unsigned integer to _Sat signed/unsigned fract.  */
#define SAT_CONV5(TYPE) \
  CONV2(TYPE, 100, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, 100, _Sat _Fract, 1.0r) \
  CONV2(TYPE, 100, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, 100, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, 100, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, 100, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, 100, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, 100, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, 1, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, 1, _Sat _Fract, 1.0r) \
  CONV2(TYPE, 1, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, 1, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, 1, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, 1, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, 1, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, 1, _Sat unsigned long long _Fract, 1.0ullr)

/* Floating-point to _Sat signed/unsigned fract.  */
#define SAT_CONV6(TYPE) \
  CONV2(TYPE, 100.0, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -100.0, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 100.0, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -100.0, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 100.0, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -100.0, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 100.0, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -100.0, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 100.0, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -100.0, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 100.0, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -100.0, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 100.0, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -100.0, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 100.0, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -100.0, _Sat unsigned long long _Fract, 0.0ullr) \
  CONV2(TYPE, 1.0, _Sat short _Fract, 1.0hr) \
  CONV2(TYPE, -1.0, _Sat short _Fract, -0.5hr - 0.5hr) \
  CONV2(TYPE, 1.0, _Sat _Fract, 1.0r) \
  CONV2(TYPE, -1.0, _Sat _Fract, -0.5r - 0.5r) \
  CONV2(TYPE, 1.0, _Sat long _Fract, 1.0lr) \
  CONV2(TYPE, -1.0, _Sat long _Fract, -0.5lr - 0.5lr) \
  CONV2(TYPE, 1.0, _Sat long long _Fract, 1.0llr) \
  CONV2(TYPE, -1.0, _Sat long long _Fract, -0.5llr - 0.5llr) \
  CONV2(TYPE, 1.0, _Sat unsigned short _Fract, 1.0uhr) \
  CONV2(TYPE, -1.0, _Sat unsigned short _Fract, 0.0uhr) \
  CONV2(TYPE, 1.0, _Sat unsigned _Fract, 1.0ur) \
  CONV2(TYPE, -1.0, _Sat unsigned _Fract, 0.0ur) \
  CONV2(TYPE, 1.0, _Sat unsigned long _Fract, 1.0ulr) \
  CONV2(TYPE, -1.0, _Sat unsigned long _Fract, 0.0ulr) \
  CONV2(TYPE, 1.0, _Sat unsigned long long _Fract, 1.0ullr) \
  CONV2(TYPE, -1.0, _Sat unsigned long long _Fract, 0.0ullr)
