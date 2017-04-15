/* PR middle-end/78461 - [7 Regression] ICE: in operator+=, at
   gimple-ssa-sprintf.c:214
   Disable warnings to exercise code paths through the pass that may
   not be exercised when the -Wformat-overflow option is in effect.  */
/* { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized -w" } */


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

int f0 (const char *s)
{
  int n = __builtin_snprintf (0, 0, "%.*s%08x", 1, s, 1);

  ASSERT (7 < n && n < 10);

  ASSERT_MAYBE (8 == n);
  ASSERT_MAYBE (9 == n);

  return n;
}

char buf[64];

int f1 (const char *s)
{
  int n = __builtin_snprintf (buf, 64, "%.*s%08x", 1, s, 1);

  ASSERT (7 < n && n < 10);

  ASSERT_MAYBE (8 == n);
  ASSERT_MAYBE (9 == n);

  return n;
}

int f2 (const char *s)
{
  int n = __builtin_snprintf (0, 0, "%.*s", 2, s);

  ASSERT (0 <= n && n <= 2);

  ASSERT_MAYBE (0 == n);
  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);

  return n;
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized"} }
   { dg-final { scan-tree-dump-times "keep_call_on_line" 7 "optimized"} } */
