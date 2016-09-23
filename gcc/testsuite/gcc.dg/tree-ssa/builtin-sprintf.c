/* Test to verify that the return value of calls to __builtin_sprintf
   that produce a known number of bytes on output is available for
   constant folding.  With optimization enabled the test will fail to
   link if any of the assertions fails.  Without optimization the test
   aborts at runtime if any of the assertions fails.  */
/* { dg-do run } */
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
      else
	__builtin_printf ("PASS: %s:%i: \"%s\" result %i: \"%s\"\n",
			  func, line, fmt, n, dst);
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

char buffer[256];
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

/* Generate a pseudo-random value in the specified range.  The return
   value must be unsigned char to work around limitations in the GCC
   range information.  Similarly for the declaration of rand() whose
   correct return value should be int, but that also prevents the range
   information from making it to the printf pass.  */

unsigned char uchar_range (unsigned min, unsigned max)
{
  extern unsigned rand (void);

  unsigned x;
  x = rand ();

  if (x < min)
    x = min;
  else if (max < x)
    x = max;

  return x;
}

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

#if __SIZEOF_SHORT__ == 2
  RNG ( 1,  6,  7, "%hi",      i);
  RNG ( 1,  5,  6, "%hu",      i);
#elif __SIZEOF_SHORT__ == 4
  RNG ( 1, 11, 12, "%hi",      i);
  RNG ( 1, 10, 11, "%hu",      i);
#endif

#if __SIZEOF_INT__ == 2
  RNG ( 1,  6,  7, "%i",       i);
  RNG ( 1,  5,  6, "%u",       i);
#elif __SIZEOF_INT__ == 4
  RNG ( 1, 11, 12, "%i",       i);
  RNG ( 1, 10, 11, "%u",       i);
#elif __SIZEOF_INT__ == 8
  RNG ( 1, 20, 21, "%i",       i);
  RNG ( 1, 19, 20, "%u",       i);
#endif

#if __SIZEOF_LONG__ == 4
  RNG ( 1, 11, 12, "%li",      li);
  RNG ( 1, 10, 11, "%lu",      li);
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
test_a_double (void)
{
  EQL ( 6,  7, "%a",   0.0);        /* 0x0p+0 */
  EQL ( 6,  7, "%a",   1.0);        /* 0x8p-3 */
  EQL ( 6,  7, "%a",   2.0);        /* 0x8p-2 */

  EQL ( 8,  9, "%.1a", 3.0);        /* 0xc.0p-2 */
  EQL ( 9, 10, "%.2a", 4.0);        /* 0xa.00p-1 */
}

static void __attribute__ ((noinline, noclone))
test_a_long_double (void)
{
  EQL ( 6,  7, "%La",   0.0L);      /* 0x0p+0 */
  EQL ( 6,  7, "%La",   1.0L);      /* 0x8p-3 */
  EQL ( 6,  7, "%La",   2.0L);      /* 0x8p-2 */

  EQL ( 8,  9, "%.1La", 3.0L);      /* 0xc.0p-2 */
  EQL ( 9, 10, "%.2La", 4.0L);      /* 0xa.00p-1 */
}

static void __attribute__ ((noinline, noclone))
test_e_double (void)
{
  EQL (12, 13, "%e",  1.0e0);
  EQL (13, 14, "%e", -1.0e0);
  EQL (12, 13, "%e",  1.0e+1);
  EQL (13, 14, "%e", -1.0e+1);
  EQL (12, 13, "%e",  1.0e+12);
  EQL (13, 14, "%e", -1.0e+12);
  EQL (13, 14, "%e",  1.0e+123);
  EQL (14, 15, "%e", -1.0e+123);

  EQL (12, 13, "%e",  9.999e+99);
  EQL (12, 13, "%e",  9.9999e+99);
  EQL (12, 13, "%e",  9.99999e+99);

  /* The actual output of the following directive depends on the rounding
     mode.  */
  /* EQL (12, "%e",  9.9999994e+99); */

  EQL (12, 13, "%e",  1.0e-1);
  EQL (12, 13, "%e",  1.0e-12);
  EQL (13, 14, "%e",  1.0e-123);
}

static void __attribute__ ((noinline, noclone))
test_e_long_double (void)
{
  EQL (12, 13, "%Le",  1.0e0L);
  EQL (13, 14, "%Le", -1.0e0L);
  EQL (12, 13, "%Le",  1.0e+1L);
  EQL (13, 14, "%Le", -1.0e+1L);
  EQL (12, 13, "%Le",  1.0e+12L);
  EQL (13, 14, "%Le", -1.0e+12L);
  EQL (13, 14, "%Le",  1.0e+123L);
  EQL (14, 15, "%Le", -1.0e+123L);

  EQL (12, 13, "%Le",  9.999e+99L);
  EQL (12, 13, "%Le",  9.9999e+99L);
  EQL (12, 13, "%Le",  9.99999e+99L);

#if __DBL_DIG__ < __LDBL_DIG__
  EQL (12, 13, "%Le",  9.999999e+99L);
#else
  RNG (12, 13, 14, "%Le",  9.999999e+99L);
#endif

  /* The actual output of the following directive depends on the rounding
     mode.  */
  /* EQL (12, "%Le",  9.9999994e+99L); */

  EQL (12, 13, "%Le",  1.0e-1L);
  EQL (12, 13, "%Le",  1.0e-12L);
  EQL (13, 14, "%Le",  1.0e-123L);

  EQL ( 6,  7, "%.0Le",   1.0e-111L);
  EQL ( 8,  9, "%.1Le",   1.0e-111L);
  EQL (19, 20, "%.12Le",  1.0e-112L);
  EQL (20, 21, "%.13Le",  1.0e-113L);
}

static void __attribute__ ((noinline, noclone))
test_f_double (void)
{
  EQL (  8,   9, "%f", 0.0e0);
  EQL (  8,   9, "%f", 0.1e0);
  EQL (  8,   9, "%f", 0.12e0);
  EQL (  8,   9, "%f", 0.123e0);
  EQL (  8,   9, "%f", 0.1234e0);
  EQL (  8,   9, "%f", 0.12345e0);
  EQL (  8,   9, "%f", 0.123456e0);
  EQL (  8,   9, "%f", 1.234567e0);

  EQL (  9,  10, "%f", 1.0e+1);
  EQL ( 20,  21, "%f", 1.0e+12);
  EQL (130, 131, "%f", 1.0e+123);

  EQL (  8,   9, "%f", 1.0e-1);
  EQL (  8,   9, "%f", 1.0e-12);
  EQL (  8,   9, "%f", 1.0e-123);
}

static void __attribute__ ((noinline, noclone))
test_f_long_double (void)
{
  EQL (  8,   9, "%Lf", 0.0e0L);
  EQL (  8,   9, "%Lf", 0.1e0L);
  EQL (  8,   9, "%Lf", 0.12e0L);
  EQL (  8,   9, "%Lf", 0.123e0L);
  EQL (  8,   9, "%Lf", 0.1234e0L);
  EQL (  8,   9, "%Lf", 0.12345e0L);
  EQL (  8,   9, "%Lf", 0.123456e0L);
  EQL (  8,   9, "%Lf", 1.234567e0L);

  EQL (  9,  10, "%Lf", 1.0e+1L);
  EQL ( 20,  21, "%Lf", 1.0e+12L);
  EQL (130, 131, "%Lf", 1.0e+123L);

  EQL (  8,   9, "%Lf", 1.0e-1L);
  EQL (  8,   9, "%Lf", 1.0e-12L);
  EQL (  8,   9, "%Lf", 1.0e-123L);
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

int main (void)
{
  test_c ('?');
  test_d_i (0xdeadbeef, 0xdeadbeefL);
  test_x ('?', 0xdead, 0xdeadbeef);

  test_a_double ();
  test_e_double ();
  test_f_double ();

  test_a_long_double ();
  test_e_long_double ();
  test_f_long_double ();

  test_s (0);

  if (nfails)
    {
      __builtin_printf ("%u out of %u tests failed\n", nfails, ntests);
      __builtin_abort ();
    }

  return 0;
}
