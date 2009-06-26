/* { dg-options "--std=gnu99" } */

/* Check that optimizations like (x * 1) to x, or (x * -1) to -x,
   do not apply to decimal float computations where trailing zeroes
   are significant.  */

extern void abort (void);
int failcnt;

#ifdef DBG
extern int printf (const char *, ...);
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

#define COMPARE32(A,B) \
  A.i == B.i

#define COMPARE64(A,B) \
  A.i[0] == B.i[0] && A.i[1] == B.i[1]

#define COMPARE128(A,B) \
  A.i[0] == B.i[0] && A.i[1] == B.i[1] && A.i[2] == B.i[2] && A.i[3] == B.i[3]

typedef union {
  _Decimal32 d;
  unsigned int i;
} u32;

typedef union {
  _Decimal64 d;
  unsigned int i[2];
} u64;

typedef union {
  _Decimal128 d;
  unsigned int i[4];
} u128;

volatile u32 p32_1;
volatile u32 p32_1_0;
volatile u32 p32_2_0;
volatile u32 m32_1;
volatile u32 m32_1_0;
volatile u32 m32_2_0;
volatile u32 a32;

volatile u64 p64_1;
volatile u64 p64_1_0;
volatile u64 p64_2_0;
volatile u64 m64_1;
volatile u64 m64_1_0;
volatile u64 m64_2_0;
volatile u64 a64;

volatile u128 p128_1;
volatile u128 p128_1_0;
volatile u128 p128_2_0;
volatile u128 m128_1;
volatile u128 m128_1_0;
volatile u128 m128_2_0;
volatile u128 a128;

void
init32 (void)
{
  p32_1.d = 1.DF;
  p32_1_0.d = 1.0DF;
  p32_2_0.d = 2.0DF;
  m32_1.d = -1.DF;
  m32_1_0.d = -1.0DF;
  m32_2_0.d = -2.0DF;
}

void
init64 (void)
{
  p64_1.d = 1.DD;
  p64_1_0.d = 1.0DD;
  p64_2_0.d = 2.0DD;
  m64_1.d = -1.DD;
  m64_1_0.d = -1.0DD;
  m64_2_0.d = -2.0DD;
}

void
init128 (void)
{
  p128_1.d = 1.DL;
  p128_1_0.d = 1.0DL;
  p128_2_0.d = 2.0DL;
  m128_1.d = -1.DL;
  m128_1_0.d = -1.0DL;
  m128_2_0.d = -2.0DL;
}

void
doit32 (void)
{
  /* Multiplying by a value with no trailing zero should not change the
     quantum exponent.  */

  a32.d = p32_2_0.d * p32_1.d;
  if (! (COMPARE32 (a32, p32_2_0)))
    FAILURE

  a32.d = p32_2_0.d * 1.DF;
  if (! (COMPARE32 (a32, p32_2_0)))
    FAILURE

  a32.d = p32_2_0.d * m32_1.d;
  if (! (COMPARE32 (a32, m32_2_0)))
    FAILURE

  a32.d = p32_2_0.d * -1.DF;
  if (! (COMPARE32 (a32, m32_2_0)))
    FAILURE

  /* Multiplying by a value with a trailing zero should change the
     quantum exponent.  */

  a32.d = p32_2_0.d * p32_1_0.d;
  if (COMPARE32 (a32, p32_2_0))
    FAILURE

  a32.d = p32_2_0.d * 1.0DF;
  if (COMPARE32 (a32, p32_2_0))
    FAILURE

  a32.d = p32_2_0.d * m32_1_0.d;
  if (COMPARE32 (a32, m32_2_0))
    FAILURE

  a32.d = p32_2_0.d * -1.0DF;
  if (COMPARE32 (a32, m32_2_0))
    FAILURE
}

void
doit64 (void)
{
  /* Multiplying by a value with no trailing zero should not change the
     quantum exponent.  */

  a64.d = p64_2_0.d * p64_1.d;
  if (! (COMPARE64 (a64, p64_2_0)))
    FAILURE

  a64.d = p64_2_0.d * 1.DD;
  if (! (COMPARE64 (a64, p64_2_0)))
    FAILURE

  a64.d = p64_2_0.d * m64_1.d;
  if (! (COMPARE64 (a64, m64_2_0)))
    FAILURE

  a64.d = p64_2_0.d * -1.DD;
  if (! (COMPARE64 (a64, m64_2_0)))
    FAILURE

  /* Multiplying by a value with a trailing zero should change the
     quantum exponent.  */

  a64.d = p64_2_0.d * p64_1_0.d;
  if (COMPARE64 (a64, p64_2_0))
    FAILURE

  a64.d = p64_2_0.d * 1.0DD;
  if (COMPARE64 (a64, p64_2_0))
    FAILURE

  a64.d = p64_2_0.d * m64_1_0.d;
  if (COMPARE64 (a64, m64_2_0))
    FAILURE

  a64.d = p64_2_0.d * -1.0DD;
  if (COMPARE64 (a64, m64_2_0))
    FAILURE
}

void
doit128 (void)
{
  /* Multiplying by a value with no trailing zero should not change the
     quantum exponent.  */

  a128.d = p128_2_0.d * p128_1_0.d;
  if (COMPARE128 (a128, p128_2_0))
    FAILURE

  a128.d = p128_2_0.d * 1.0DD;
  if (COMPARE128 (a128, p128_2_0))
    FAILURE

  a128.d = p128_2_0.d * m128_1_0.d;
  if (COMPARE128 (a128, m128_2_0))
    FAILURE

  a128.d = p128_2_0.d * -1.0DD;
  if (COMPARE128 (a128, m128_2_0))
    FAILURE

  /* Multiplying by a value with a trailing zero should change the
     quantum exponent.  */

  a128.d = p128_2_0.d * p128_1.d;
  if (! (COMPARE128 (a128, p128_2_0)))
    FAILURE

  a128.d = p128_2_0.d * 1.DD;
  if (! (COMPARE128 (a128, p128_2_0)))
    FAILURE

  a128.d = p128_2_0.d * m128_1.d;
  if (! (COMPARE128 (a128, m128_2_0)))
    FAILURE

  a128.d = p128_2_0.d * -1.DD;
  if (! (COMPARE128 (a128, m128_2_0)))
    FAILURE
}

int
main (void)
{
  init32 ();
  init64 ();
  init128 ();

  doit32 ();
  doit64 ();
  doit128 ();

  if (failcnt != 0)
    abort ();

  return 0;
}
