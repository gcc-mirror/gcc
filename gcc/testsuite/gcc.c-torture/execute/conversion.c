/* Test front-end conversions, optimizer conversions, and run-time
   conversions between different arithmetic types.

   Constants are specified in a non-obvious way to make them work for
   any word size.  Their value on a 32-bit machine is indicated in the
   comments.

   Note that this code is NOT intended for testing of accuracy of fp
   conversions.  */

float
u2f(u)
     unsigned int u;
{
  return u;
}

double
u2d(u)
     unsigned int u;
{
  return u;
}

float
s2f(s)
     int s;
{
  return s;
}

double
s2d(s)
     int s;
{
  return s;
}

int
fnear (float x, float y)
{
  float t = x - y;
  return t == 0 || x / t > 1000000.0;
}

int
dnear (double x, double y)
{
  double t = x - y;
  return t == 0 || x / t > 100000000000000.0;
}

test_integer_to_float()
{
  if (u2f(0U) != (float) 0U)				/* 0 */
    abort();
  if (!fnear (u2f(~0U), (float) ~0U))			/* 0xffffffff */
    abort();
  if (!fnear (u2f((~0U) >> 1), (float) ((~0U) >> 1)))	/* 0x7fffffff */
    abort();
  if (u2f(~((~0U) >> 1)) != (float) ~((~0U) >> 1))	/* 0x80000000 */
    abort();

  if (u2d(0U) != (double) 0U)				/* 0 */
    abort();
  if (!dnear (u2d(~0U), (double) ~0U))			/* 0xffffffff */
    abort();
  if (!dnear (u2d((~0U) >> 1),(double) ((~0U) >> 1)))	/* 0x7fffffff */
    abort();
  if (u2d(~((~0U) >> 1)) != (double) ~((~0U) >> 1))	/* 0x80000000 */
    abort();

  if (s2f(0) != (float) 0)				/* 0 */
    abort();
  if (!fnear (s2f(~0), (float) ~0))			/* 0xffffffff */
    abort();
  if (!fnear (s2f((int)((~0U) >> 1)), (float)(int)((~0U) >> 1))) /* 0x7fffffff */
    abort();
  if (s2f((int)(~((~0U) >> 1))) != (float)(int)~((~0U) >> 1)) /* 0x80000000 */
    abort();

  if (s2d(0) != (double) 0)				/* 0 */
    abort();
  if (!dnear (s2d(~0), (double) ~0))			/* 0xffffffff */
    abort();
  if (!dnear (s2d((int)((~0U) >> 1)), (double)(int)((~0U) >> 1))) /* 0x7fffffff */
    abort();
  if (s2d((int)~((~0U) >> 1)) != (double)(int)~((~0U) >> 1)) /* 0x80000000 */
    abort();
}

#if __GNUC__
float
ull2f(u)
     unsigned long long int u;
{
  return u;
}

double
ull2d(u)
     unsigned long long int u;
{
  return u;
}

float
sll2f(s)
     long long int s;
{
  return s;
}

double
sll2d(s)
     long long int s;
{
  return s;
}

test_longlong_integer_to_float()
{
  if (ull2f(0ULL) != (float) 0ULL)			/* 0 */
    abort();
  if (ull2f(~0ULL) != (float) ~0ULL)			/* 0xffffffff */
    abort();
  if (ull2f((~0ULL) >> 1) != (float) ((~0ULL) >> 1))	/* 0x7fffffff */
    abort();
  if (ull2f(~((~0ULL) >> 1)) != (float) ~((~0ULL) >> 1)) /* 0x80000000 */
    abort();

  if (ull2d(0ULL) != (double) 0ULL)			/* 0 */
    abort();
#if __HAVE_68881__
  /* Some 68881 targets return values in fp0, with excess precision.
     But the compile-time conversion to double works correctly.  */
  if (! dnear (ull2d(~0ULL), (double) ~0ULL))		/* 0xffffffff */
    abort();
  if (! dnear (ull2d((~0ULL) >> 1), (double) ((~0ULL) >> 1))) /* 0x7fffffff */
    abort();
#else
  if (ull2d(~0ULL) != (double) ~0ULL)			/* 0xffffffff */
    abort();
  if (ull2d((~0ULL) >> 1) != (double) ((~0ULL) >> 1))	/* 0x7fffffff */
    abort();
#endif
  if (ull2d(~((~0ULL) >> 1)) != (double) ~((~0ULL) >> 1)) /* 0x80000000 */
    abort();

  if (sll2f(0LL) != (float) 0LL)			/* 0 */
    abort();
  if (sll2f(~0LL) != (float) ~0LL)			/* 0xffffffff */
    abort();
  if (! fnear (sll2f((long long int)((~0ULL) >> 1)), (float)(long long int)((~0ULL) >> 1))) /* 0x7fffffff */
    abort();
  if (sll2f((long long int)(~((~0ULL) >> 1))) != (float)(long long int)~((~0ULL) >> 1)) /* 0x80000000 */
    abort();

  if (sll2d(0LL) != (double) 0LL)			/* 0 */
    abort();
  if (sll2d(~0LL) != (double) ~0LL)			/* 0xffffffff */
    abort();
  if (!dnear (sll2d((long long int)((~0ULL) >> 1)), (double)(long long int)((~0ULL) >> 1))) /* 0x7fffffff */
    abort();
  if (! dnear (sll2d((long long int)~((~0ULL) >> 1)), (double)(long long int)~((~0ULL) >> 1))) /* 0x80000000 */
    abort();
}
#endif

unsigned int
f2u(float f)
{
  return (unsigned) f;
}

unsigned int
d2u(double d)
{
  return (unsigned) d;
}

int
f2s(float f)
{
  return (int) f;
}

int
d2s(double d)
{
  return (int) d;
}

test_float_to_integer()
{
  if (f2u(0.0) != 0)
    abort();
  if (f2u(0.999) != 0)
    abort();
  if (f2u(1.0) != 1)
    abort();
  if (f2u(1.99) != 1)
    abort();
  if (f2u((float) ((~0U) >> 1)) != (~0U) >> 1 &&	/* 0x7fffffff */
      f2u((float) ((~0U) >> 1)) != ((~0U) >> 1) + 1)
    abort();
  if (f2u((float) ~((~0U) >> 1)) != ~((~0U) >> 1))	/* 0x80000000 */
    abort();

 /* These tests require double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (double) >= 8) {
  if (d2u(0.0) != 0)
    abort();
  if (d2u(0.999) != 0)
    abort();
  if (d2u(1.0) != 1)
    abort();
  if (d2u(1.99) != 1)
    abort();
  if (d2u((double) (~0U)) != ~0U)			/* 0xffffffff */
    abort();
  if (d2u((double) ((~0U) >> 1)) != (~0U) >> 1)		/* 0x7fffffff */
    abort();
  if (d2u((double) ~((~0U) >> 1)) != ~((~0U) >> 1))	/* 0x80000000 */
    abort();
 }

  if (f2s(0.0) != 0)
    abort();
  if (f2s(0.999) != 0)
    abort();
  if (f2s(1.0) != 1)
    abort();
  if (f2s(1.99) != 1)
    abort();
  if (f2s(-0.999) != 0)
    abort();
  if (f2s(-1.0) != -1)
    abort();
  if (f2s(-1.99) != -1)
    abort();
  if (f2s((float)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
    abort();

 /* These tests require double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (double) >= 8) {
  if (d2s(0.0) != 0)
    abort();
  if (d2s(0.999) != 0)
    abort();
  if (d2s(1.0) != 1)
    abort();
  if (d2s(1.99) != 1)
    abort();
  if (d2s(-0.999) != 0)
    abort();
  if (d2s(-1.0) != -1)
    abort();
  if (d2s(-1.99) != -1)
    abort();
  if (d2s((double) ((~0U) >> 1)) != (~0U) >> 1)		/* 0x7fffffff */
    abort();
  if (d2s((double)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
    abort();
 }
}

#if __GNUC__
unsigned long long int
f2ull(float f)
{
  return (unsigned long long int) f;
}

unsigned long long int
d2ull(double d)
{
  return (unsigned long long int) d;
}

long long int
f2sll(float f)
{
  return (long long int) f;
}

long long int
d2sll(double d)
{
  return (long long int) d;
}

test_float_to_longlong_integer()
{
  if (f2ull(0.0) != 0LL)
    abort();
  if (f2ull(0.999) != 0LL)
    abort();
  if (f2ull(1.0) != 1LL)
    abort();
  if (f2ull(1.99) != 1LL)
    abort();
  if (f2ull((float) ((~0ULL) >> 1)) != (~0ULL) >> 1 &&	/* 0x7fffffff */
      f2ull((float) ((~0ULL) >> 1)) != ((~0ULL) >> 1) + 1)
    abort();
  if (f2ull((float) ~((~0ULL) >> 1)) != ~((~0ULL) >> 1)) /* 0x80000000 */
    abort();

  if (d2ull(0.0) != 0LL)
    abort();
  if (d2ull(0.999) != 0LL)
    abort();
  if (d2ull(1.0) != 1LL)
    abort();
  if (d2ull(1.99) != 1LL)
    abort();
  if (d2ull((double) ((~0ULL) >> 1)) != (~0ULL) >> 1 &&	/* 0x7fffffff */
      d2ull((double) ((~0ULL) >> 1)) != ((~0ULL) >> 1) + 1)
    abort();
  if (d2ull((double) ~((~0ULL) >> 1)) != ~((~0ULL) >> 1)) /* 0x80000000 */
    abort();


  if (f2sll(0.0) != 0LL)
    abort();
  if (f2sll(0.999) != 0LL)
    abort();
  if (f2sll(1.0) != 1LL)
    abort();
  if (f2sll(1.99) != 1LL)
    abort();
  if (f2sll(-0.999) != 0LL)
    abort();
  if (f2sll(-1.0) != -1LL)
    abort();
  if (f2sll(-1.99) != -1LL)
    abort();
  if (f2sll((float)(long long int)~((~0ULL) >> 1)) != (long long int)~((~0ULL) >> 1)) /* 0x80000000 */
    abort();

  if (d2sll(0.0) != 0LL)
    abort();
  if (d2sll(0.999) != 0LL)
    abort();
  if (d2sll(1.0) != 1LL)
    abort();
  if (d2sll(1.99) != 1LL)
    abort();
  if (d2sll(-0.999) != 0LL)
    abort();
  if (d2sll(-1.0) != -1LL)
    abort();
  if (d2sll(-1.99) != -1LL)
    abort();
  if (d2sll((double)(long long int)~((~0ULL) >> 1)) != (long long int)~((~0ULL) >> 1)) /* 0x80000000 */
    abort();
}
#endif

main()
{
  test_integer_to_float();
  test_float_to_integer();
#if __GNUC__
  test_longlong_integer_to_float();
  test_float_to_longlong_integer();
#endif
  exit(0);
}
