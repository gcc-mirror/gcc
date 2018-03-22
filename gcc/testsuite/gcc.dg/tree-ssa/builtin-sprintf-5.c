/* PR middle-end/78476 - snprintf(0, 0, ...) with known arguments not
   optimized away
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" }
   { dg-require-effective-target int32plus } */

#define CAT(s, n)   s ## n
#define FAIL(line)  CAT (failure_on_line_, line)
#define PASS(line)  CAT (success_on_line_, line)

/* Emit a call to a function named failure_on_line_NNN when VALUE
   is not equal to the constant EXPECTED, otherwise emit a call to
   function success_on_line_NNN.  */
#define ASSERT(value, expected)			\
  do {						\
    extern void FAIL (__LINE__)(int);		\
    extern void PASS (__LINE__)(int);		\
    if (value == expected)			\
      PASS (__LINE__)(value);			\
    else					\
      FAIL (__LINE__)(value);			\
  } while (0)

/* Verify that EXPECT == snprintf(0, 0, ...).  */
#define EQL(expect, ...)				\
  do {							\
    int n = __builtin_snprintf (0, 0, __VA_ARGS__);	\
    ASSERT (n, expect);					\
  } while (0)

static int ival (int i) { return i; }

/* Generate a signed int value in the specified range.  */

static int
int_range (int min, int max)
{
  extern int int_value (void);
  int val = int_value ();
  if (val < min || max < val)
    val = min;
  return val;
}

#define R(min, max) int_range (min, max)

void test_arg_int (int i, int n)
{
  EQL (1, "%i", ival (0));
  EQL (1, "%i", ival (1));
  EQL (2, "%i%i", ival (0), ival (1));
  EQL (3, "%i%i%i", ival (0), ival (1), ival (9));
  EQL (5, "%i %i %i", ival (0), ival (1), ival (9));

  EQL (5, "%i %i %i", ival (0), ival (1), ival (9));

  EQL (13, "%hhu.%hhu.%hhu.%hhu", ival (23), ival (78), ival (216), ival (135));

  for (i = 0; i != 9; ++i)
    EQL (1, "%i", i);

  for (i = -n; i != n; ++i)
    EQL (8, "%08x", i);

  /*  As a special case, a precision of zero with an argument of zero
      results in zero bytes (unless modified by width).  */
  EQL (0, "%.0d", ival (0));
  EQL (0, "%.0i", ival (0));
  EQL (0, "%.0o", ival (0));
  EQL (0, "%.0u", ival (0));
  EQL (0, "%.0x", ival (0));

  EQL (0, "%.*d", 0, ival (0));
  EQL (0, "%.*i", 0, ival (0));
  EQL (0, "%.*o", 0, ival (0));
  EQL (0, "%.*u", 0, ival (0));
  EQL (0, "%.*x", 0, ival (0));

  EQL (1, "%1.0d", ival (0));
  EQL (1, "%1.0i", ival (0));
  EQL (1, "%1.0o", ival (0));
  EQL (1, "%1.0u", ival (0));
  EQL (1, "%1.0x", ival (0));

  EQL (4, "%hhi", R (-128, -127));
  EQL (3, "%hhi", R ( -99,  -10));
  EQL (2, "%hhi", R (  -9,   -1));
  EQL (1, "%hhi", R (   0,    9));
  EQL (1, "%hhi", R (   0,    9));

  EQL (1, "%1.0hhi", R (   0,    1));
  EQL (1, "%1.1hhi", R (   0,    9));
  EQL (2, "%1.2hhi", R (   0,    9));
  EQL (3, "%1.3hhi", R (   0,    9));

  EQL (1, "%hhi", R (1024, 1033));
  EQL (2, "%hhi", R (1034, 1123));
  EQL (1, "%hhu", R (1024, 1033));
  EQL (2, "%hhu", R (1034, 1123));
}

void test_arg_string (const char *s)
{
  EQL ( 0, "%-s", "");
  EQL ( 1, "%%");
  EQL ( 1, "%-s", "1");
  EQL ( 2, "%-s", "12");
  EQL ( 3, "%-s", "123");
  EQL ( 5, "s=%s", "123");
  EQL (10, "%%s=\"%s\"", "12345");

  EQL ( 1, "%.*s", 1, "123");
  EQL ( 2, "%.*s", 2, "123");
  EQL ( 3, "%.*s", 3, "123");
  EQL ( 3, "%.*s", 4, "123");

  EQL ( 1, "%1.*s", 1, "123");
  EQL ( 2, "%1.*s", 2, "123");
  EQL ( 3, "%1.*s", 3, "123");
  EQL ( 3, "%1.*s", 4, "123");
  EQL ( 4, "%4.*s", 1, "123");
  EQL ( 4, "%4.*s", 2, "123");
  EQL ( 4, "%4.*s", 3, "123");
  EQL ( 4, "%4.*s", 4, "123");
  EQL ( 4, "%4.*s", 5, "123");

  const char *a = "123";
  const char *b = "456";

  EQL ( 3, "%-s", s ? a : b);
  EQL ( 0, "%.0s", s);
  EQL ( 1, "%1.1s", s);
  EQL ( 2, "%2.2s", s);
  EQL ( 2, "%2.1s", s);
}

void test_arg_multiarg (int i, double d)
{
  EQL (16, "%12i %s", i, "abc");
  EQL (16, "%*i %s", 12, i, "abc");
}

/* Verify that EXPECT == vsnprintf(0, 0, ...).  */
#define EQLv(expect, fmt, va)				\
  do {							\
    int n = __builtin_vsnprintf (0, 0, fmt, va);	\
    ASSERT (n, expect);					\
  } while (0)

void test_va_int (__builtin_va_list va)
{
  EQLv ( 2, "%02hhx", va);
  EQLv ( 4, "%04hx", va);
}

void test_va_multiarg (__builtin_va_list va)
{
  EQLv ( 8, "%8x", va);
  EQLv ( 8, "% 8x", va);
  EQLv ( 9, "%9x", va);
  EQLv (11, "%11o", va);
  EQLv (12, "%12o", va);

  EQLv (16, "%12i %3.2s", va);
}


/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized"} }
   { dg-final { scan-tree-dump-not "snprintf" "optimized"} } */
