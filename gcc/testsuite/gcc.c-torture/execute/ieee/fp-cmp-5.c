/* Like fp-cmp-4.c, but test that the setcc patterns are correct.  */

static int
test_isunordered(double x, double y)
{
  return __builtin_isunordered(x, y);
}

static int
test_not_isunordered(double x, double y)
{
  return !__builtin_isunordered(x, y);
}

static int
test_isless(double x, double y)
{
  return __builtin_isless(x, y);
}

static int
test_not_isless(double x, double y)
{
  return !__builtin_isless(x, y);
}

static int
test_islessequal(double x, double y)
{
  return __builtin_islessequal(x, y);
}

static int
test_not_islessequal(double x, double y)
{
  return !__builtin_islessequal(x, y);
}

static int
test_isgreater(double x, double y)
{
  return __builtin_isgreater(x, y);
}

static int
test_not_isgreater(double x, double y)
{
  return !__builtin_isgreater(x, y);
}

static int
test_isgreaterequal(double x, double y)
{
  return __builtin_isgreaterequal(x, y);
}

static int
test_not_isgreaterequal(double x, double y)
{
  return !__builtin_isgreaterequal(x, y);
}

static int
test_islessgreater(double x, double y)
{
  return __builtin_islessgreater(x, y);
}

static int
test_not_islessgreater(double x, double y)
{
  return !__builtin_islessgreater(x, y);
}

static void
one_test(double x, double y, int expected,
         int (*pos) (double, double), int (*neg) (double, double))
{
  if ((*pos)(x, y) != expected)
    abort ();
  if ((*neg)(x, y) != !expected)
    abort ();
}

#define NAN (0.0 / 0.0)

int
main()
{
  struct try
  {
    double x, y;
    int result[6];
  };

  static struct try const data[] =
  {
    { NAN, NAN, { 1, 0, 0, 0, 0, 0 } },
    { 0.0, NAN, { 1, 0, 0, 0, 0, 0 } },
    { NAN, 0.0, { 1, 0, 0, 0, 0, 0 } },
    { 0.0, 0.0, { 0, 0, 1, 0, 1, 0 } },
    { 1.0, 2.0, { 0, 1, 1, 0, 0, 1 } },
    { 2.0, 1.0, { 0, 0, 0, 1, 1, 1 } },
  };

  struct test
  {
    int (*pos)(double, double);
    int (*neg)(double, double);
  };

  static struct test const tests[] =
  {
    { test_isunordered, test_not_isunordered },
    { test_isless, test_not_isless },
    { test_islessequal, test_not_islessequal },
    { test_isgreater, test_not_isgreater },
    { test_isgreaterequal, test_not_isgreaterequal },
    { test_islessgreater, test_not_islessgreater }
  };

  const int n = sizeof(data) / sizeof(data[0]);
  int i, j;

  for (i = 0; i < n; ++i)
    for (j = 0; j < 6; ++j)
      one_test (data[i].x, data[i].y, data[i].result[j],
		tests[j].pos, tests[j].neg);

  exit (0);
}
