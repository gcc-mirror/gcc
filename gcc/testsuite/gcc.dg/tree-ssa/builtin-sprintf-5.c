/* PR middle-end/78476 - snprintf(0, 0, ...) with known arguments not
   optimized away
   { dg-compile }
   { dg-options "-O2 -fdump-tree-optimized" }
   { dg-require-effective-target int32plus } */

#define CAT(s, n)   s ## n
#define FAIL(line)  CAT (failure_on_line_, line)
#define PASS(line)  CAT (success_on_line_, line)

/* Emit a call to a function named failure_on_line_NNN when EXPR is false.  */
#define ASSERT(value, expect)			\
  do {						\
    extern void FAIL (__LINE__)(int);		\
    extern void PASS (__LINE__)(int);		\
    if (value == expect)			\
      PASS (__LINE__)(value);			\
    else					\
      FAIL (__LINE__)(value);			\
  } while (0)

#define T(expect, ...)					\
  do {							\
    int n = __builtin_snprintf (0, 0, __VA_ARGS__);	\
    ASSERT (n, expect);					\
  } while (0)

int ival (int i) { return i; }

void test_arg_int (int i, int n)
{
  T (1, "%i", ival (0));
  T (1, "%i", ival (1));
  T (2, "%i%i", ival (0), ival (1));
  T (3, "%i%i%i", ival (0), ival (1), ival (9));
  T (5, "%i %i %i", ival (0), ival (1), ival (9));

  T (5, "%i %i %i", ival (0), ival (1), ival (9));

  T (13, "%hhu.%hhu.%hhu.%hhu", ival (23), ival (78), ival (216), ival (135));

  for (i = 0; i != 9; ++i)
    T (1, "%i", i);

  for (i = -n; i != n; ++i)
    T (8, "%08x", i);

  /*  As a special case, a precision of zero with an argument of zero
      results in zero bytes (unless modified by width).  */
  T (0, "%.0d", ival (0));
  T (0, "%.0i", ival (0));
  T (0, "%.0o", ival (0));
  T (0, "%.0u", ival (0));
  T (0, "%.0x", ival (0));

  T (0, "%.*d", 0, ival (0));
  T (0, "%.*i", 0, ival (0));
  T (0, "%.*o", 0, ival (0));
  T (0, "%.*u", 0, ival (0));
  T (0, "%.*x", 0, ival (0));

  T (1, "%1.0d", ival (0));
  T (1, "%1.0i", ival (0));
  T (1, "%1.0o", ival (0));
  T (1, "%1.0u", ival (0));
  T (1, "%1.0x", ival (0));
}

void test_arg_string (const char *s)
{
  T ( 0, "%-s", "");
  T ( 1, "%%");
  T ( 1, "%-s", "1");
  T ( 2, "%-s", "12");
  T ( 3, "%-s", "123");
  T ( 5, "s=%s", "123");
  T (10, "%%s=\"%s\"", "12345");

  T ( 1, "%.*s", 1, "123");
  T ( 2, "%.*s", 2, "123");
  T ( 3, "%.*s", 3, "123");
  T ( 3, "%.*s", 4, "123");

  T ( 1, "%1.*s", 1, "123");
  T ( 2, "%1.*s", 2, "123");
  T ( 3, "%1.*s", 3, "123");
  T ( 3, "%1.*s", 4, "123");
  T ( 4, "%4.*s", 1, "123");
  T ( 4, "%4.*s", 2, "123");
  T ( 4, "%4.*s", 3, "123");
  T ( 4, "%4.*s", 4, "123");
  T ( 4, "%4.*s", 5, "123");

  const char *a = "123";
  const char *b = "456";

  T ( 3, "%-s", s ? a : b);
  T ( 0, "%.0s", s);
  T ( 1, "%1.1s", s);
  T ( 2, "%2.2s", s);
  T ( 2, "%2.1s", s);
}

void test_arg_multiarg (int i, double d)
{
  T (16, "%i %f %s", 123, 3.14, "abc");
  T (16, "%12i %s", i, "abc");
  T (16, "%*i %s", 12, i, "abc");
}

#define TV(expect, fmt, va)				\
  do {							\
    int n = __builtin_vsnprintf (0, 0, fmt, va);	\
    ASSERT (n, expect);					\
  } while (0)

void test_va_int (__builtin_va_list va)
{
  TV ( 2, "%02hhx", va);
  TV ( 2, "%02.*hhx", va);
  TV ( 4, "%04hx", va);
  TV ( 4, "%04.*hx", va);
}

void test_va_multiarg (__builtin_va_list va)
{
  TV ( 8, "%8x", va);
  TV ( 8, "% 8x", va);
  TV ( 9, "%9x", va);
  TV (11, "%11o", va);
  TV (12, "%12o", va);

  TV (16, "%12i %3.2s", va);
}


/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized"} }
   { dg-final { scan-tree-dump-not "snprintf" "optimized"} } */
