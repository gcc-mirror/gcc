void abort (void);
void exit (int);

#ifndef FLOAT
#define FLOAT double
#endif

/* Like fp-cmp-4.c, but test that the cmove patterns are correct.  */

static FLOAT
test_isunordered(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_isunordered(x, y) ? a : b;
}

static FLOAT
test_not_isunordered(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_isunordered(x, y) ? a : b;
}

static FLOAT
test_isless(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_isless(x, y) ? a : b;
}

static FLOAT
test_not_isless(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_isless(x, y) ? a : b;
}

static FLOAT
test_islessequal(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_islessequal(x, y) ? a : b;
}

static FLOAT
test_not_islessequal(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_islessequal(x, y) ? a : b;
}

static FLOAT
test_isgreater(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_isgreater(x, y) ? a : b;
}

static FLOAT
test_not_isgreater(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_isgreater(x, y) ? a : b;
}

static FLOAT
test_isgreaterequal(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_isgreaterequal(x, y) ? a : b;
}

static FLOAT
test_not_isgreaterequal(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_isgreaterequal(x, y) ? a : b;
}

static FLOAT
test_islessgreater(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return __builtin_islessgreater(x, y) ? a : b;
}

static FLOAT
test_not_islessgreater(FLOAT x, FLOAT y, FLOAT a, FLOAT b)
{
  return !__builtin_islessgreater(x, y) ? a : b;
}

static void
one_test(FLOAT x, FLOAT y, int expected,
         FLOAT (*pos) (FLOAT, FLOAT, FLOAT, FLOAT), 
	 FLOAT (*neg) (FLOAT, FLOAT, FLOAT, FLOAT))
{
  if (((*pos)(x, y, 1.0, 2.0) == 1.0) != expected)
    abort ();
  if (((*neg)(x, y, 3.0, 4.0) == 4.0) != expected)
    abort ();
}

#define NAN (0.0 / 0.0)
#define INF (1.0 / 0.0)

int
main()
{
  struct try
  {
    FLOAT x, y;
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
    { INF, 0.0, { 0, 0, 0, 1, 1, 1 } },
    { 1.0, INF, { 0, 1, 1, 0, 0, 1 } },
    { INF, INF, { 0, 0, 1, 0, 1, 0 } },
    { 0.0, -INF, { 0, 0, 0, 1, 1, 1 } },
    { -INF, 1.0, { 0, 1, 1, 0, 0, 1 } },
    { -INF, -INF, { 0, 0, 1, 0, 1, 0 } },
    { INF, -INF, { 0, 0, 0, 1, 1, 1 } },
    { -INF, INF, { 0, 1, 1, 0, 0, 1 } },
  };

  struct test
  {
    FLOAT (*pos)(FLOAT, FLOAT, FLOAT, FLOAT);
    FLOAT (*neg)(FLOAT, FLOAT, FLOAT, FLOAT);
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
