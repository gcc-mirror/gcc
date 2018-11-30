/* PR tree-optimization/79327 - wrong code at -O2 and -fprintf-return-value
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

void test_hho_cst (void)
{
  ASSERT (1 == __builtin_snprintf (0, 0, "%#.1hho", 0));
  ASSERT (2 == __builtin_snprintf (0, 0, "%#.2hho", 0));

  ASSERT (2 == __builtin_snprintf (0, 0, "%#.1hho", 1));
  ASSERT (2 == __builtin_snprintf (0, 0, "%#.2hho", 1));
  ASSERT (3 == __builtin_snprintf (0, 0, "%#.3hho", 1));
}

int test_hho_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#hho", i);

  ASSERT (0 < n && n < 5);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);

  return n;
}

int test_ho_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#ho", i);

  ASSERT (0 < n && n < 8);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);
  ASSERT_MAYBE (5 == n);
  ASSERT_MAYBE (6 == n);
  ASSERT_MAYBE (7 == n);

  return n;
}

int test_o_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#o", i);

  ASSERT (0 < n && n < 13);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);
  ASSERT_MAYBE (5 == n);
  ASSERT_MAYBE (6 == n);
  ASSERT_MAYBE (7 == n);
  /* Below will be optimized out for 16-bit int.  */
  ASSERT_MAYBE (8 == n);
  ASSERT_MAYBE (9 == n);
  ASSERT_MAYBE (10 == n);
  ASSERT_MAYBE (11 == n);
  ASSERT_MAYBE (12 == n);

  return n;
}

void test_hhx_cst (void)
{
  ASSERT (1 == __builtin_snprintf (0, 0, "%#.1hhx", 0));
  ASSERT (2 == __builtin_snprintf (0, 0, "%#.2hhx", 0));

  ASSERT (3 == __builtin_snprintf (0, 0, "%#.1hhx", 1));
  ASSERT (4 == __builtin_snprintf (0, 0, "%#.2hhx", 1));
  ASSERT (5 == __builtin_snprintf (0, 0, "%#.3hhx", 1));
}

int test_hhx_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#hhx", i);

  ASSERT (0 < n && n < 5);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);

  return n;
}

void test_hx_cst (void)
{
  ASSERT (1 == __builtin_snprintf (0, 0, "%#.1hx", 0));
  ASSERT (2 == __builtin_snprintf (0, 0, "%#.2hx", 0));

  ASSERT (3 == __builtin_snprintf (0, 0, "%#.1hx", 1));
  ASSERT (4 == __builtin_snprintf (0, 0, "%#.2hx", 1));
  ASSERT (5 == __builtin_snprintf (0, 0, "%#.3hx", 1));
}

int test_hx_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#hx", i);

  ASSERT (0 < n && n < 7);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);
  ASSERT_MAYBE (5 == n);
  ASSERT_MAYBE (6 == n);

  return n;
}

int test_x_var (int i)
{
  int n = __builtin_snprintf (0, 0, "%#x", i);

  ASSERT (0 < n && n < 11);

  ASSERT_MAYBE (1 == n);
  ASSERT_MAYBE (2 == n);
  ASSERT_MAYBE (3 == n);
  ASSERT_MAYBE (4 == n);
  ASSERT_MAYBE (5 == n);
  ASSERT_MAYBE (6 == n);
  /* Below will be optimized out for 16-bit int.  */
  ASSERT_MAYBE (7 == n);
  ASSERT_MAYBE (8 == n);
  ASSERT_MAYBE (9 == n);
  ASSERT_MAYBE (10 == n);

  return n;
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized"} }
   { dg-final { scan-tree-dump-times "keep_call_on_line" 43 "optimized" { target { ! int16 } } } }
   { dg-final { scan-tree-dump-times "keep_call_on_line" 34 "optimized" { target int16 } } } */
