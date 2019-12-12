/* Test to verify that the return value of calls to __builtin_sprintf
   that produce a known number of bytes on output is available for
   constant folding.  With optimization enabled the test will fail to
   link if any of the assertions fails.  Without optimization the test
   aborts at runtime if any of the assertions fails.  */
/* { dg-do run } */
/* { dg-skip-if "not IEEE float layout" { "pdp11-*-*" } } */
/* { dg-additional-options "-O2 -Wall -Wno-pedantic -fprintf-return-value" } */

#ifndef LINE
#  define LINE   0
#endif

#if __STDC_VERSION__ < 199901L
#  define __func__   __FUNCTION__
#endif

typedef __SIZE_TYPE__ size_t;

unsigned ntests;
unsigned nfails;

void __attribute__ ((noclone, noinline))
checkv (const char *func, int line, int res, int min, int max,
	char *dst, const char *fmt, __builtin_va_list va)
{
  int n = __builtin_vsprintf (dst, fmt, va);
  int len = __builtin_strlen (dst);

  ++ntests;

  int fail = 0;
  if (n != res)
    {
      __builtin_printf ("FAIL: %s:%i: \"%s\" expected result for \"%s\" "
			"doesn't match function call return value: ",
			func, line, fmt, dst);
      if (min == max)
	__builtin_printf ("%i != %i\n", n, min);
      else
	__builtin_printf ("%i not in [%i, %i]\n", n, min, max);

      fail = 1;
    }
  else
    {
      if (len < min || max < len)
	{
	  __builtin_printf ("FAIL: %s:%i: \"%s\" expected result for \"%s\" "
			    "doesn't match output length: ",
			    func, line, fmt, dst);

	  if (min == max)
	    __builtin_printf ("%i != %i\n", len, min);
	  else
	    __builtin_printf ("%i not in [%i, %i]\n", len, min, max);

	  fail = 1;
	}
      else if (min == max)
	__builtin_printf ("PASS: %s:%i: \"%s\" result %i: \"%s\"\n",
			  func, line, fmt, n, dst);
      else
	__builtin_printf ("PASS: %s:%i: \"%s\" result %i in [%i, %i]: \"%s\"\n",
			  func, line, fmt, n, min, max, dst);
    }

  if (fail)
    ++nfails;
}

void __attribute__ ((noclone, noinline))
check (const char *func, int line, int res, int min, int max,
       char *dst, const char *fmt, ...)
{
  __builtin_va_list va;
  __builtin_va_start (va, fmt);
  checkv (func, line, res, min, max, dst, fmt, va);
  __builtin_va_end (va);
}

char buffer[4100];
char* volatile dst = buffer;
char* ptr = buffer;

#define concat(a, b)   a ## b
#define CAT(a, b)      concat (a, b)

#if __OPTIMIZE__
/* With optimization references to the following undefined symbol which
   is unique for each test case are expected to be eliminated.  */
#  define TEST_FAILURE(line, ignore1, ignore2, ignore3)			\
  do {									\
    extern void CAT (failure_on_line_, line)(void);			\
    CAT (failure_on_line_, line)();					\
  } while (0)
#else
/* The test is run by DejaGnu with optimization enabled.  When it's run
   with it disabled (i.e., at -O0) each test case is verified at runtime
   and the test aborts just before exiting if any of them failed.  */
#  define TEST_FAILURE(line, result, min, max)				\
  if (min == max)							\
    __builtin_printf ("FAIL: %s:%i: expected %i, got %i\n",		\
		      __func__, line, min, result);			\
  else									\
    __builtin_printf ("FAIL: %s:%i: expected range [%i, %i], got %i\n",	\
		      __func__, line, min, max, result);
#endif

/* Verify that the result is exactly equal to RES.  */
#define EQL(expect, size, fmt, ...)					\
  if (!LINE || LINE == __LINE__)					\
    do {								\
      char *buf = (size) < 0 ? ptr : buffer + sizeof buffer - (size);	\
      int result = __builtin_sprintf (buf, fmt, __VA_ARGS__);		\
      if (result != expect)						\
	{								\
	  TEST_FAILURE (__LINE__, expect, expect, result);		\
	}								\
      check (__func__, __LINE__, result, expect, expect, dst, fmt,	\
	     __VA_ARGS__);						\
    } while (0)

/* Verify that the result is in the range [MIN, MAX].  */
#define RNG(min, max, size, fmt, ...)					\
  if (!LINE || LINE == __LINE__)					\
    do {								\
      char *buf = (size) < 0 ? ptr : buffer + sizeof buffer - (size);	\
      int result = __builtin_sprintf (buf, fmt, __VA_ARGS__);		\
      if (result < min || max < result)					\
	{								\
	  TEST_FAILURE (__LINE__, min, max, result);			\
	}								\
      check (__func__, __LINE__, result, min, max, dst, fmt,		\
	     __VA_ARGS__);						\
    } while (0)

static void __attribute__ ((noinline, noclone))
test_c (char c)
{
  EQL (1,  2, "%c",       c);
  EQL (1, -1, "%c",       c);
  EQL (1,  2, "%1c",      c);
  EQL (1, -1, "%1c",      c);
  EQL (1,  2, "%*c",      1, c);
  EQL (1, -1, "%*c",      1, c);
  EQL (2,  3, "%c%c",     '1', '2');
  EQL (2, -1, "%c%c",     '1', '2');
  EQL (3,  4, "%3c",      c);
  EQL (3, -1, "%3c",      c);
  EQL (3,  4, "%*c",      3, c);
  EQL (3, -1, "%*c",      3, c);

  EQL (3,  4, "%*c%*c",     2,  c,  1,  c);
  EQL (3,  4, "%*c%*c",     1,  c,  2,  c);
  EQL (3,  4, "%c%c%c",        '1',    '2',    '3');
  EQL (3,  4, "%*c%c%c",    1, '1',    '2',    '3');
  EQL (3,  4, "%*c%*c%c",   1, '1', 1, '2',    '3');
  EQL (3,  4, "%*c%*c%*c",  1, '1', 1, '2', 1, '3');

  EQL (3, -1, "%*c%*c",     2,  c,  1,  c);
  EQL (3, -1, "%*c%*c",     1,  c,  2,  c);
  EQL (3, -1, "%c%c%c",        '1',    '2',    '3');
  EQL (3, -1, "%*c%c%c",    1, '1',    '2',    '3');
  EQL (3, -1, "%*c%*c%c",   1, '1', 1, '2',    '3');
  EQL (3, -1, "%*c%*c%*c",  1, '1', 1, '2', 1, '3');

  EQL (4,  5, "%c%c %c",  '1', '2', '3');
  EQL (5,  6, "%c %c %c", '1', '2', '3');
  EQL (5,  6, "%c %c %c",  c,   c,   c);
}

/* Generate a pseudo-random unsigned value.  */

unsigned __attribute__ ((noclone, noinline))
unsigned_value (void)
{
  extern int rand ();
  return rand ();
}

/* Generate a pseudo-random signed value.  */

int __attribute__ ((noclone, noinline))
int_value (void)
{
  extern int rand ();
  return rand ();
}

/* Generate an unsigned char value in the specified range.  */

static unsigned char
uchar_range (unsigned min, unsigned max)
{
  unsigned x = unsigned_value ();
  if (x < min || max < x)
    x = min;
  return x;
}

/* Generate a signed int value in the specified range.  */

static int
int_range (int min, int max)
{
  int val = int_value ();
  if (val < min || max < val)
    val = min;
  return val;
}

#define IR(min, max) int_range (min, max)

static void __attribute__ ((noinline, noclone))
test_d_i (int i, long li)
{
  /*    +-------------------------- expected return value */
  /*    |   +---------------------- destination size */
  /*    |   |  +------------------- format string */
  /*    |   |  |                +-- variable argument(s) */
  /*    |   |  |                | */
  /*    V   V  V                V */
  EQL ( 1,  2, "%d",            0);
  EQL ( 2,  3, "%d%d",          0,   1);
  EQL ( 3,  4, "%d%d",          9,  10);
  EQL ( 4,  5, "%d%d",         11,  12);
  EQL ( 5,  6, "%d:%d",        12,  34);
  EQL ( 5,  6, "%d",           12345);
  EQL ( 6,  7, "%d",          -12345);
  EQL (15, 16, "%d:%d:%d:%d", 123, 124, 125, 126);

  EQL ( 1,  2, "%i", uchar_range (0, 9));
  EQL ( 1, -1, "%i", uchar_range (0, 9));

  /* The range information available to passes other than the Value
     Range Propoagation pass itself is so bad that the following two
     tests fail (the range seen in the test below is [0, 99] rather
     than [10, 99].
  EQL ( 2,  3, "%i", uchar_range (10, 99));
  EQL ( 3,  4, "%i", uchar_range (100, 199));
  */

  /* Verify that the width allows the return value in the following
     calls can be folded despite the unknown value of the argument.  */
#if __SIZEOF_INT__ == 2
  EQL ( 6,  7, "%6d",      i);
  EQL ( 6,  7, "%+6d",     i);
  EQL ( 6,  7, "%-6d",     i);
  EQL ( 6,  7, "%06d",     i);
#elif __SIZEOF_INT__ == 4
  EQL (11, 12, "%11d",     i);
  EQL (11, 12, "%+11d",    i);
  EQL (11, 12, "%-11d",    i);
  EQL (11, 12, "%011d",    i);
#elif __SIZEOF_INT__ == 8
  EQL (20, 21, "%20d",     i);
  EQL (20, 21, "%+20d",    i);
  EQL (20, 21, "%-29d",    i);
  EQL (20, 21, "%020d",    i);
#endif

#if __SIZEOF_LONG__ == 2
  EQL ( 6,  7, "%6ld",      li);
  EQL ( 6,  7, "%+6ld",     li);
  EQL ( 6,  7, "%-6ld",     li);
  EQL ( 6,  7, "%06ld",     li);
#elif __SIZEOF_LONG__ == 4
  EQL (11, 12, "%11ld",     li);
  EQL (11, 12, "%+11ld",    li);
  EQL (11, 12, "%-11ld",    li);
  EQL (11, 12, "%011ld",    li);
#elif __SIZEOF_LONG__ == 8
  EQL (20, 21, "%20ld",     li);
  EQL (20, 21, "%+20ld",    li);
  EQL (20, 21, "%-20ld",    li);
  EQL (20, 21, "%020ld",    li);
#endif

  /* Verify that the output of a directive with an unknown argument
     is correctly determined at compile time to be in the expected
     range.  */

  /*    +---------------------------- expected minimum return value */
  /*    |   +------------------------ expected maximum return value */
  /*    |   |   +-------------------- destination size */
  /*    |   |   |  +----------------- format string */
  /*    |   |   |  |           +----- variable argument(s) */
  /*    |   |   |  |           | */
  /*    V   V   V  V           V */
  RNG ( 1,  4,  5, "%hhi",     i);
  RNG ( 1,  3,  4, "%hhu",     i);

  RNG ( 3,  4,  5, "%hhi",     IR (-128,  -10));
  RNG ( 2,  4,  5, "%hhi",     IR (-128,   -1));
  RNG ( 1,  4,  5, "%hhi",     IR (-128,    0));

  RNG ( 1,  4,  5, "%1hhi",    IR (-128,    0));
  RNG ( 1,  4,  5, "%2hhi",    IR (-128,    0));
  RNG ( 1,  4,  5, "%3hhi",    IR (-128,    0));
  RNG ( 1,  4,  5, "%4hhi",    IR (-128,    0));
  RNG ( 1,  5,  6, "%5hhi",    IR (-128,    0));
  RNG ( 1,  6,  7, "%6hhi",    IR (-128,    0));
  RNG ( 2,  6,  7, "%6hhi",    IR (-128,   10));

  RNG ( 0,  1,  2, "%.hhi",    IR (   0,    1));
  RNG ( 0,  1,  2, "%.0hhi",   IR (   0,    1));
  RNG ( 0,  1,  2, "%0.0hhi",  IR (   0,    1));   /* { dg-warning ".0. flag ignored with precision" } */
  RNG ( 0,  1,  2, "%*.0hhi",  0, IR (   0,    1));

  RNG ( 1,  2,  3, "%hhi",     IR (1024, 1034));
  RNG ( 1,  4,  5, "%hhi",     IR (1024, 2048));
  RNG ( 2,  3,  4, "%hhi",     IR (1034, 1151));

  RNG ( 1,  2,  3, "%hhu",     IR (1024, 1034));
  RNG ( 1,  3,  4, "%hhu",     IR (1024, 2048));
  RNG ( 2,  3,  4, "%hhu",     IR (1034, 1151));

#if __SIZEOF_SHORT__ == 2
  RNG ( 1,  6,  7, "%hi",      i);
  RNG ( 1,  5,  6, "%hu",      i);
  RNG ( 1,  6,  7, "%.1hi",    i);
  RNG ( 2,  6,  7, "%.2hi",    i);
  RNG ( 3,  6,  7, "%.3hi",    i);
  RNG ( 4,  6,  7, "%.4hi",    i);
  RNG ( 5,  6,  7, "%.5hi",    i);
  RNG ( 6,  7,  8, "%.6hi",    i);
  RNG ( 7,  8,  9, "%.7hi",    i);
#elif __SIZEOF_SHORT__ == 4
  RNG ( 1, 11, 12, "%hi",      i);
  RNG ( 1, 10, 11, "%hu",      i);

  RNG ( 1, 11, 12, "%.1hi",    i);
  RNG ( 2, 11, 12, "%.2hi",    i);
  RNG ( 3, 11, 12, "%.3hi",    i);
  RNG ( 4, 11, 12, "%.4hi",    i);
  RNG ( 5, 11, 12, "%.5hi",    i);
  RNG ( 6, 11, 12, "%.6hi",    i);
  RNG ( 7, 11, 12, "%.7hi",    i);
  RNG ( 8, 11, 12, "%.8hi",    i);
  RNG ( 9, 11, 12, "%.9hi",    i);
  RNG (10, 11, 12, "%.10hi",   i);
  RNG (11, 12, 13, "%.11hi",   i);
  RNG (12, 13, 14, "%.12hi",   i);
  RNG (13, 14, 15, "%.13hi",   i);
#endif

#if __SIZEOF_INT__ == 2
  RNG ( 1,  6,  7, "%i",       i);
  RNG ( 1,  5,  6, "%u",       i);

  RNG ( 1,  6,  7, "%.1i",     i);
  RNG ( 2,  6,  7, "%.2i",     i);
  RNG ( 3,  6,  7, "%.3i",     i);
  RNG ( 4,  6,  7, "%.4i",     i);
  RNG ( 5,  6,  7, "%.5i",     i);
  RNG ( 6,  7,  8, "%.6i",     i);
  RNG ( 7,  8,  9, "%.7i",     i);
#elif __SIZEOF_INT__ == 4
  RNG ( 1, 11, 12, "%i",       i);
  RNG ( 1, 10, 11, "%u",       i);

  RNG ( 1, 11, 12, "%.1i",    i);
  RNG ( 2, 11, 12, "%.2i",    i);
  RNG ( 3, 11, 12, "%.3i",    i);
  RNG ( 4, 11, 12, "%.4i",    i);
  RNG ( 5, 11, 12, "%.5i",    i);
  RNG ( 6, 11, 12, "%.6i",    i);
  RNG ( 7, 11, 12, "%.7i",    i);
  RNG ( 8, 11, 12, "%.8i",    i);
  RNG ( 9, 11, 12, "%.9i",    i);
  RNG (10, 11, 12, "%.10i",   i);
  RNG (11, 12, 13, "%.11i",   i);
  RNG (12, 13, 14, "%.12i",   i);
  RNG (13, 14, 15, "%.13i",   i);
#elif __SIZEOF_INT__ == 8
  RNG ( 1, 20, 21, "%i",       i);
  RNG ( 1, 19, 20, "%u",       i);
#endif

#if __SIZEOF_LONG__ == 4
  RNG ( 1, 11, 12, "%li",      li);
  RNG ( 1, 10, 11, "%lu",      li);

  RNG ( 1, 11, 12, "%.1li",    li);
  RNG ( 2, 11, 12, "%.2li",    li);
  RNG ( 3, 11, 12, "%.3li",    li);
  RNG ( 4, 11, 12, "%.4li",    li);
  RNG ( 5, 11, 12, "%.5li",    li);
  RNG ( 6, 11, 12, "%.6li",    li);
  RNG ( 7, 11, 12, "%.7li",    li);
  RNG ( 8, 11, 12, "%.8li",    li);
  RNG ( 9, 11, 12, "%.9li",    li);
  RNG (10, 11, 12, "%.10li",   li);
  RNG (11, 12, 13, "%.11li",   li);
  RNG (12, 13, 14, "%.12li",   li);
  RNG (13, 14, 15, "%.13li",   li);
#elif __SIZEOF_LONG__ == 8
  RNG ( 1, 20, 21, "%li",      li);
  RNG ( 1, 19, 20, "%lu",      li);
#endif
}

static void __attribute__ ((noinline, noclone))
test_x (unsigned char uc, unsigned short us, unsigned ui)
{
  EQL ( 1,  2, "%hhx",          0);
  EQL ( 2,  3, "%2hhx",         0);
  EQL ( 2,  3, "%02hhx",        0);
  EQL ( 2,  3, "%#02hhx",       0);

  EQL ( 1,  2, "%hhx",          1);
  EQL ( 2,  3, "%2hhx",         1);
  EQL ( 2,  3, "%02hhx",        1);
  EQL ( 3,  4, "%#02hhx",       1);

  EQL ( 2,  3, "%2hhx",        uc);
  EQL ( 2,  3, "%02hhx",       uc);
  EQL ( 5,  6, "%#05hhx",      uc);

  EQL ( 2,  3, "%2hhx",        us);
  EQL ( 2,  3, "%02hhx",       us);
  EQL ( 5,  6, "%#05hhx",      us);

  EQL ( 2,  3, "%2hhx",        ui);
  EQL ( 2,  3, "%02hhx",       ui);
  EQL ( 5,  6, "%#05hhx",      ui);

  EQL ( 1,  2, "%x",            0);
  EQL ( 1,  2, "%#x",           0);
  EQL ( 1,  2, "%#0x",          0);
  EQL ( 1,  2, "%x",            1);
  EQL ( 1,  2, "%x",          0xf);
  EQL ( 2,  3, "%x",         0x10);
  EQL ( 2,  3, "%x",         0xff);
  EQL ( 3,  4, "%x",        0x100);

  EQL (11, 12, "%02x:%02x:%02x:%02x",         0xde, 0xad, 0xbe, 0xef);

  /* The following would be optimized if the range information of
  the variable's type was made available.  Alas, it's lost due
  to the promotion of the actual argument (unsined char) to
  the type of the "formal" argument (int in the case of the
  ellipsis).
  EQL (11, 12, "%02x:%02x:%02x:%02x",   uc,   uc,   uc,   uc);
  */
  EQL (11, 12, "%02hhx:%02hhx:%02hhx:%02hhx",   uc,   uc,   uc,   uc);

#if __SIZEOF_SHORT__ == 2
  EQL ( 4,  5, "%04hx",                   us);
  EQL ( 9, 10, "%04hx:%04hx",             us, us);
  EQL (14, 15, "%04hx:%04hx:%04hx",       us, us, us);
  EQL (19, 20, "%04hx:%04hx:%04hx:%04hx", us, us, us, us);
#endif

#if __SIZEOF_INT__ == 2
  EQL ( 4,  5, "%04x", ui);
  EQL ( 6,  7, "%#06x", ui);
#elif __SIZEOF_INT__ == 4
  EQL ( 8,  9, "%08x", ui);
  EQL (10, 10 + 1, "%#010x", ui);
#elif __SIZEOF_INT__ == 8
  EQL (16, 17, "%016x", ui);
  EQL (18, 19, "%#018x",  ui);
#endif
}

static void __attribute__ ((noinline, noclone))
test_a_double (double d)
{
  EQL ( 6,  7, "%.0a", 0.0);        /* 0x0p+0 */
  EQL ( 6,  7, "%.0a", 1.0);        /* 0x8p-3 */
  EQL ( 6,  7, "%.0a", 2.0);        /* 0x8p-2 */

  /* The decimal point may be up to MB_LEN_MAX long.  */
  RNG ( 8, 13, 14, "%.1a", 3.0);    /* 0xc.0p-2 */
  RNG ( 9, 14, 15, "%.2a", 4.0);    /* 0x8.00p-1 */
  RNG (10, 15, 16, "%.3a", 5.0);    /* 0xa.000p-1 */

  RNG (11, 16, 17, "%.*a", 4, 6.0); /* 0xc.0000p-1 */
  RNG (12, 17, 18, "%.*a", 5, 7.0); /* 0xe.00000p-1 */
	                            /* d is in [ 0, -DBL_MAX ] */
  RNG ( 3, 10, 11, "%.0a", d);      /* inf/nan or 0x0p+0 ... -0x2p+1023 */
  /* %a is poorly specified and allows for implementations divergence:
     some (such as Glibc) trim redundant trailing zeros after decimal
     point and others (e.g., Solaris) don't.  */
  RNG ( 3, 30, 31, "%.1a", d);      /* inf or 0x0.0p+0  ... -0x2.0...0p+1023 */
  RNG ( 3, 30, 31, "%.2a", d);      /* inf or 0x0.00p+0 ... -0x2.00...0p+1023 */
}

static void __attribute__ ((noinline, noclone))
test_a_long_double (void)
{
  EQL ( 6,  7, "%.0La", 0.0L);      /* 0x0p+0 */
  EQL ( 6,  7, "%.0La", 1.0L);      /* 0x8p-3 */
  EQL ( 6,  7, "%.0La", 2.0L);      /* 0x8p-2 */

  RNG ( 8, 13, 14, "%.1La", 3.0L);  /* 0xc.0p-2 */
  RNG ( 9, 14, 15, "%.2La", 4.0L);  /* 0xa.00p-1 */
}

static void __attribute__ ((noinline, noclone))
test_e_double (double d)
{
  RNG (12, 17, 18, "%e",  1.0e0);
  RNG (13, 18, 19, "%e", -1.0e0);
  RNG (12, 17, 18, "%e",  1.0e+1);
  RNG (13, 18, 19, "%e", -1.0e+1);
  RNG (12, 17, 18, "%e",  1.0e+12);
  RNG (13, 18, 19, "%e", -1.0e+12);
  RNG (13, 18, 19, "%e",  1.0e+123);
  RNG (14, 19, 20, "%e", -1.0e+123);

  RNG (12, 17, 18, "%e",  9.999e+99);
  RNG (12, 17, 18, "%e",  9.9999e+99);
  RNG (12, 17, 18, "%e",  9.99999e+99);

  /* The actual output of the following directive depends on the rounding
     mode.  */
  /* RNG (12, "%e",  9.9999994e+99); */

  RNG (12, 17, 18, "%e",  1.0e-1);
  RNG (12, 17, 18, "%e",  1.0e-12);
  RNG (13, 18, 19, "%e",  1.0e-123);

  RNG ( 3, 19, 20, "%e",   d);
  RNG ( 3, 11, 12, "%.e",  d);
  RNG ( 3, 12, 13, "%.0e", d);
  RNG ( 3, 14, 15, "%.1e", d);
  RNG ( 3, 15, 16, "%.2e", d);
  RNG ( 3, 16, 17, "%.3e", d);
  RNG ( 3, 17, 18, "%.4e", d);
  RNG ( 3, 18, 19, "%.5e", d);
  RNG ( 3, 19, 20, "%.6e", d);
  RNG ( 3, 20, 21, "%.7e", d);

  RNG ( 3, 4013, 4014, "%.4000e", d);

  RNG ( 3,  7,  8, "%.*e", 0, d);
  RNG ( 3, 14, 15, "%.*e", 1, d);
  RNG ( 3, 15, 16, "%.*e", 2, d);
  RNG ( 3, 16, 17, "%.*e", 3, d);
  RNG ( 3, 17, 18, "%.*e", 4, d);
  RNG ( 3, 18, 19, "%.*e", 5, d);
  RNG ( 3, 19, 20, "%.*e", 6, d);
  RNG ( 3, 20, 21, "%.*e", 7, d);

  RNG ( 3, 4013, 4014, "%.*e",  4000, d);
  RNG ( 4, 4013, 4014, "%+.*e", 4000, d);
  RNG ( 4, 4013, 4014, "% .*e", 4000, d);
  RNG ( 3, 4013, 4014, "%#.*e", 4000, d);
}

static void __attribute__ ((noinline, noclone))
test_e_long_double (long double d)
{
  RNG (12, 17, 18, "%Le",  1.0e0L);
  RNG (13, 18, 19, "%Le", -1.0e0L);
  RNG (12, 17, 18, "%Le",  1.0e+1L);
  RNG (13, 18, 19, "%Le", -1.0e+1L);
  RNG (12, 18, 19, "%Le",  1.0e+12L);
  RNG (13, 19, 20, "%Le", -1.0e+12L);
  RNG (13, 19, 20, "%Le",  1.0e+123L);
  RNG (14, 20, 21, "%Le", -1.0e+123L);

  RNG (12, 18, 19, "%Le",  9.999e+99L);
  RNG (12, 18, 19, "%Le",  9.9999e+99L);
  RNG (12, 18, 19, "%Le",  9.99999e+99L);

#if __DBL_DIG__ < __LDBL_DIG__
  RNG (12, 17, 18, "%Le",  9.999999e+99L);
#else
  RNG (12, 18, 19, "%Le",  9.999999e+99L);
#endif

  /* The actual output of the following directive depends on the rounding
     mode.  */
  /* RNG (12, "%Le",  9.9999994e+99L); */

  RNG (12, 17, 18, "%Le",  1.0e-1L);
  RNG (12, 17, 18, "%Le",  1.0e-12L);
  RNG (13, 18, 19, "%Le",  1.0e-123L);

  EQL ( 6,  7, "%.0Le",   1.0e-111L);

  RNG ( 8, 13, 14, "%.1Le",   1.0e-111L);
  RNG (19, 25, 25, "%.12Le",  1.0e-112L);
  RNG (20, 26, 27, "%.13Le",  1.0e-113L);

  /* The following correspond to the double results plus 1 for the upper
     bound accounting for the four-digit exponent.  The lower bound is
     for inf/nan.  */
  RNG ( 3, 20, 21, "%Le", d);    /* inf or 0.000000e+00 ...  -1.189732e+4932 */
  RNG ( 3,  8,  9, "%.Le", d);
  RNG ( 3,  9, 10, "%.0Le", d);
  RNG ( 3, 15, 16, "%.1Le", d);  /* inf or 0.0e+00      ...  -1.2e+4932 */
  RNG ( 3, 16, 17, "%.2Le", d);  /* inf or 0.00e+00     ...  -1.19e+4932 */
  RNG ( 3, 17, 18, "%.3Le", d);
  RNG ( 3, 18, 19, "%.4Le", d);
  RNG ( 3, 19, 20, "%.5Le", d);
  RNG ( 3, 20, 21, "%.6Le", d);  /* same as plain "%Le" */
  RNG ( 3, 21, 22, "%.7Le", d);  /* inf or 0.0000000e+00 ... -1.1897315e+4932 */

  RNG ( 3,  9, 10, "%.*Le", 0, d);
  RNG ( 3, 15, 16, "%.*Le", 1, d);
  RNG ( 3, 16, 17, "%.*Le", 2, d);
  RNG ( 3, 17, 18, "%.*Le", 3, d);
  RNG ( 3, 18, 19, "%.*Le", 4, d);
  RNG ( 3, 19, 20, "%.*Le", 5, d);
  RNG ( 3, 20, 21, "%.*Le", 6, d);
  RNG ( 3, 21, 22, "%.*Le", 7, d);
}

static void __attribute__ ((noinline, noclone))
test_f_double (double d)
{
  RNG (  8,  13,  14, "%f", 0.0e0);
  RNG (  8,  13,  14, "%f", 0.1e0);
  RNG (  8,  13,  14, "%f", 0.12e0);
  RNG (  8,  13,  14, "%f", 0.123e0);
  RNG (  8,  13,  14, "%f", 0.1234e0);
  RNG (  8,  13,  14, "%f", 0.12345e0);
  RNG (  8,  13,  14, "%f", 0.123456e0);
  RNG (  8,  13,  14, "%f", 1.234567e0);

  RNG (  9,  14,  15, "%f", 1.0e+1);
  RNG ( 20,  26,  27, "%f", 1.0e+12);
  RNG (130, 136, 137, "%f", 1.0e+123);

  RNG (  8,  13,  14, "%f", 1.0e-1);
  RNG (  8,  13,  14, "%f", 1.0e-12);
  RNG (  8,  13,  14, "%f", 1.0e-123);

  RNG (  3, 322, 323, "%f",  d);
  RNG (  4, 322, 323, "%+f", d);
  RNG (  4, 322, 323, "% f", d);
  RNG (  3, 322, 323, "%#f", d);
}

static void __attribute__ ((noinline, noclone))
test_f_long_double (void)
{
  RNG (  8,  15,  16, "%Lf", 0.0e0L);
  RNG (  8,  14,  15, "%Lf", 0.1e0L);
  RNG (  8,  14,  15, "%Lf", 0.12e0L);
  RNG (  8,  14,  15, "%Lf", 0.123e0L);
  RNG (  8,  14,  15, "%Lf", 0.1234e0L);
  RNG (  8,  14,  15, "%Lf", 0.12345e0L);
  RNG (  8,  14,  15, "%Lf", 0.123456e0L);
  RNG (  8,  14,  15, "%Lf", 1.234567e0L);

  RNG (  9,  15,  16, "%Lf", 1.0e+1L);
  RNG ( 20,  26,  27, "%Lf", 1.0e+12L);
  RNG (130, 136, 137, "%Lf", 1.0e+123L);

  RNG (  8,  14,  15, "%Lf", 1.0e-1L);
  RNG (  8,  14,  15, "%Lf", 1.0e-12L);
  RNG (  8,  14,  15, "%Lf", 1.0e-123L);
}

static void __attribute__ ((noinline, noclone))
test_g_double (double d)
{
  /* Numbers exactly representable in binary floating point.  */
  EQL (  1,   2, "%g", 0.0);

  RNG (  3,   8,  9, "%g", 1.0 / 2);
  RNG (  4,   9, 10, "%g", 1.0 / 4);
  RNG (  5,  10, 11, "%g", 1.0 / 8);
  RNG (  6,  11, 12, "%g", 1.0 / 16);
  RNG (  7,  12, 13, "%g", 1.0 / 32);
  RNG (  8,  13, 14, "%g", 1.0 / 64);
  RNG (  9,  14, 15, "%g", 1.0 / 128);
  RNG ( 10,  15, 16, "%g", 1.0 / 256);
  RNG ( 10,  16, 17, "%g", 1.0 / 512);

  /* Numbers that are not exactly representable.  */
  RNG ( 3, 13, 14, "%g", 0.1);
  RNG ( 4, 13, 14, "%g", 0.12);
  RNG ( 5, 13, 14, "%g", 0.123);
  RNG ( 6, 13, 14, "%g", 0.1234);
  RNG ( 7, 13, 14, "%g", 0.12345);
  RNG ( 8, 13, 14, "%g", 0.123456);

  RNG ( 4, 17, 18, "%g", 0.123e+1);
  RNG ( 8, 18, 19, "%g", 0.123e+12);
  RNG ( 9, 19, 20, "%g", 0.123e+134);

  RNG ( 1, 18, 19, "%g", d);
  RNG ( 1, 12, 13, "%.g", d);
  RNG ( 1, 12, 13, "%.0g", d);
  RNG ( 1, 12, 13, "%.1g", d);
  RNG ( 1, 14, 15, "%.2g", d);
  RNG ( 1, 15, 16, "%.3g", d);
  RNG ( 1, 16, 17, "%.4g", d);
  RNG ( 1, 17, 18, "%.5g", d);
  RNG ( 1, 18, 19, "%.6g", d);
  RNG ( 1, 19, 20, "%.7g", d);
  RNG ( 1, 20, 21, "%.8g", d);

  RNG ( 1, 315, 316, "%.9999g", d);

  RNG ( 1, 12, 13, "%.*g", 0, d);
  RNG ( 1, 12, 13, "%.*g", 1, d);
  RNG ( 1, 14, 15, "%.*g", 2, d);
  RNG ( 1, 15, 16, "%.*g", 3, d);
  RNG ( 1, 16, 17, "%.*g", 4, d);
  RNG ( 1, 17, 18, "%.*g", 5, d);
  RNG ( 1, 18, 19, "%.*g", 6, d);
  RNG ( 1, 19, 20, "%.*g", 7, d);
  RNG ( 1, 20, 21, "%.*g", 8, d);

  RNG ( 1, 315, 316, "%.*g", 9999, d);
}

static void __attribute__ ((noinline, noclone))
test_g_long_double (void)
{
  /* Numbers exactly representable in binary floating point.  */
  EQL (  1,   2, "%Lg", 0.0L);
  RNG (  3,   8, 9, "%Lg", 1.0L / 2);
  RNG (  4,   9, 10, "%Lg", 1.0L / 4);
  RNG (  5,  10, 11, "%Lg", 1.0L / 8);
  RNG (  6,  11, 12, "%Lg", 1.0L / 16);
  RNG (  7,  12, 13, "%Lg", 1.0L / 32);
  RNG (  8,  13, 14, "%Lg", 1.0L / 64);
  RNG (  9,  14, 15, "%Lg", 1.0L / 128);
  RNG ( 10,  15, 16, "%Lg", 1.0L / 256);
  RNG ( 10,  15, 16, "%Lg", 1.0L / 512);

  /* Numbers that are not exactly representable.  */

  /* The following test case results in up to 14 bytes on powerpc*-*-*
     but only in 13 bytes on x86_64 (see PR testsuite/79293).  Test just
     for the former for simplicity.  */
  RNG ( 3, 14, 15, "%Lg", 0.1L);

  RNG ( 4, 13, 14, "%Lg", 0.12L);
  RNG ( 5, 13, 14, "%Lg", 0.123L);
  RNG ( 6, 13, 14, "%Lg", 0.1234L);
  RNG ( 7, 13, 14, "%Lg", 0.12345L);
  RNG ( 8, 13, 14, "%Lg", 0.123456L);

  RNG ( 4, 12, 13, "%Lg", 0.123e+1L);
  RNG ( 8, 13, 14, "%Lg", 0.123e+12L);
  RNG ( 9, 17, 18, "%Lg", 0.123e+134L);
}

static void __attribute__ ((noinline, noclone))
test_s (int i)
{
  EQL (  0,   1, "%s", "");
  EQL (  0,   1, "%s", "\0");
  EQL (  1,   2, "%1s", "");
  EQL (  1,   2, "%s", "1");
  EQL (  2,   3, "%2s", "");
  EQL (  2,   3, "%s", "12");
  EQL (  2,   3, "%s%s", "12", "");
  EQL (  2,   3, "%s%s", "", "12");
  EQL (  2,   3, "%s%s", "1", "2");
  EQL (  3,   4, "%3s", "");
  EQL (  3,   4, "%3s", "1");
  EQL (  3,   4, "%3s", "12");
  EQL (  3,   4, "%3s", "123");
  EQL (  3,   4, "%3.3s", "1");
  EQL (  3,   4, "%3.3s", "12");
  EQL (  3,   4, "%3.3s", "123");
  EQL (  3,   4, "%3.3s", "1234");
  EQL (  3,   4, "%3.3s", "12345");
  EQL (  3,   4, "%s %s", "1", "2");
  EQL (  4,   5, "%s %s", "12", "3");
  EQL (  5,   6, "%s %s", "12", "34");
  EQL (  5,   6, "[%s %s]", "1", "2");
  EQL (  6,   7, "[%s %s]", "12", "3");
  EQL (  7,   8, "[%s %s]", "12", "34");

  /* Verify the result of a conditional expression involving string
     literals is in the expected range of their lengths.  */
  RNG (  0,   3,   4, "%-s", i ? ""    : "123");
  RNG (  1,   4,   5, "%-s", i ? "1"   : "1234");
  RNG (  2,   5,   6, "%-s", i ? "12"  : "12345");
  RNG (  3,   6,   7, "%-s", i ? "123" : "123456");
}

static void __attribute__ ((noinline, noclone))
test_n (void)
{
  int n;
  EQL (  0,   1, "%n", &n);
  EQL (  1,   2, "1%n", &n);
  EQL (  2,   3, "12%n", &n);
  EQL (  3,   4, "12%n3", &n);
  EQL (  4,   5, "12%n34", &n);
  EQL (  4,   5, "12%n34%n", &n, &n);
  EQL (  5,   6, "12%n34%n5", &n, &n);
  EQL (  6,   7, "12%n34%n56", &n, &n);
  EQL (  6,   7, "%s%n%s%n%s", "12", &n, "34", &n, "56");
}

static void __attribute__ ((noinline, noclone))
test_percent (void)
{
  /* Provide extra arguments siunce the EQL macro needs at least one.  */
  EQL (  1,   2, "%%", 0);         /* { dg-warning "too many arguments" } */
  EQL (  2,   3, "%%%%", 0);       /* { dg-warning "too many arguments" } */
  EQL (  3,   4, "%%%%%%", 0);     /* { dg-warning "too many arguments" } */
  EQL (  3,   4, "%%%%%%%s", "");
  EQL (  3,   4, "%%%%%s%%", "");
  EQL (  3,   4, "%%%s%%%%", "");
  EQL (  3,   4, "%s%%%%%%", "");
}

int main (void)
{
  test_c ('?');
  test_d_i (0xdeadbeef, 0xdeadbeefL);
  test_x ('?', 0xdead, 0xdeadbeef);

  test_a_double (0.0);
  test_e_double (0.0);
  test_f_double (0.0);
  test_g_double (0.0);

  test_a_long_double ();
  test_e_long_double (0.0);
  test_f_long_double ();
  test_g_long_double ();

  test_s (0);

  test_n ();

  test_percent ();

  if (nfails)
    {
      __builtin_printf ("%u out of %u tests failed\n", nfails, ntests);
      __builtin_abort ();
    }

  return 0;
}
