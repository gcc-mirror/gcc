/* PR tree-optimization/79376 - wrong lower bound with %s and non-constant
   strings in -Wformat-overflow
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

#define CAT(s, n)   s ## n
#define FAIL(line)  CAT (failure_on_line_, line)

/* Emit a call to a function named failure_on_line_NNN when EXPR is false.  */
#define ASSERT(expr)				\
  do {						\
    extern void FAIL (__LINE__)(void);		\
    if (!(expr)) FAIL (__LINE__)();		\
  } while (0)

#define KEEP(line)  CAT (keep_call_on_line_, line)

/* Emit a call to a function named keep_call_on_line_NNN when EXPR is true.
   Used to verify that the expression need not be the only one that holds.  */
#define ASSERT_MAYBE(expr)			\
  do {						\
    extern void KEEP (__LINE__)(void);		\
    if (expr) KEEP (__LINE__)();		\
  } while (0)

struct Arrays
{
  char a1[1];
  char a2[2];
  char a3[3];
  char a4[4];
  char a5[5];
  char ax[];
};

void test_arrays (int i, struct Arrays *a)
{
  {
    const char *s = i < 0 ? a->a3 : a->a1;

    int n = __builtin_snprintf (0, 0, "%-s", s);

    /* Since it's undefined to pass an unterminated array to a %s
       directive it would be valid to assume that S above is not
       longer than sizeof (A->A3) but the optimization isn't done
       because the GIMPLE representation of the %s argument isn't
       suffficiently reliable not to confuse it for some other
       array.  The argument length is therefore assumed to be in
       the range [0, PTRDIFF_MAX - 2] and the sprintf result to be
       as big as INT_MAX and possibly even negative if the function
       were to fail due to a single directive resulting in more than
       the 4,095 byte maximum required to be supported.
       ASSERT (0 <= n && n < 3);
    */

    ASSERT_MAYBE (0 == n);
    ASSERT_MAYBE (1 == n);
    ASSERT_MAYBE (2 == n);
  }

  {
    const char *s = i < 0 ? a->a3 : a->a5;

    int n = __builtin_snprintf (0, 0, "%-s", s);

    /* ASSERT (0 <= n && n < 5); */

    ASSERT_MAYBE (0 == n);
    ASSERT_MAYBE (1 == n);
    ASSERT_MAYBE (2 == n);
    ASSERT_MAYBE (3 == n);
    ASSERT_MAYBE (4 == n);
  }
}

void test_string_and_array (int i, struct Arrays *a)
{
  {
    const char *s = i < 0 ? a->a3 : "1";

    int n = __builtin_snprintf (0, 0, "%-s", s);

    /* ASSERT (0 <= n && n < 3); */

    ASSERT_MAYBE (0 == n);
    ASSERT_MAYBE (1 == n);
    ASSERT_MAYBE (2 == n);
  }

  {
    const char *s = i < 0 ? "12" : a->a5;

    int n = __builtin_snprintf (0, 0, "%-s", s);

    /* ASSERT (0 <= n && n < 5); */

    ASSERT_MAYBE (0 == n);
    ASSERT_MAYBE (1 == n);
    ASSERT_MAYBE (2 == n);
    ASSERT_MAYBE (3 == n);
    ASSERT_MAYBE (4 == n);
  }

  {
    const char *s = i < 0 ? a->a4 : 0 < i ? "12" : a->a5;

    int n = __builtin_snprintf (0, 0, "%-s", s);

    /* ASSERT (0 <= n && n < 5); */

    ASSERT_MAYBE (0 == n);
    ASSERT_MAYBE (1 == n);
    ASSERT_MAYBE (2 == n);
    ASSERT_MAYBE (3 == n);
    ASSERT_MAYBE (4 == n);
  }
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized"} }
   { dg-final { scan-tree-dump-times "keep_call_on_line" 21 "optimized"} } */
