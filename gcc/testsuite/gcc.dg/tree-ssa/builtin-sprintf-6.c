/* PR middle-end/78476 - snprintf(0, 0, ...) with known arguments not
   optimized away
   A negative test complementing builtin-sprintf-5.c to verify that calls
   to the function that do not return a constant are not optimized away.
   { dg-compile }
   { dg-options "-O2 -fdump-tree-optimized" }
   { dg-require-effective-target int32plus } */

#define CONCAT(a, b) a ## b
#define CAT(a, b)    CONCAT (a, b)

#define T(...)								\
  do {									\
    int CAT (n, __LINE__) = __builtin_snprintf (0, 0, __VA_ARGS__);	\
    sink (CAT (n, __LINE__));						\
  } while (0)

void sink (int);

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

void test_arg_int (int width, int prec, int i, int n)
{
  T ("%i", i);
  T ("%1i", i);
  T ("%2i", i);
  T ("%3i", i);
  T ("%4i", i);

  T ("%*i", width, 0);
  T ("%*i", width, 1);
  T ("%*i", width, i);

  T ("%.*i", prec, 0);
  T ("%.*i", prec, 1);
  T ("%.*i", prec, i);
  T ("%.*i", 0,    i);

  T ("%i", R (1, 10));

  for (i = -n; i != n; ++i)
    T ("%*x", n, i);
}

void test_arg_string (int width, int prec, const char *s)
{
  T ("%-s", s);
  T ("%1s", s);
  T ("%.1s", s);
  T ("%*s", width, s);
  T ("%.*s", prec, s);
  T ("%1.*s", prec, s);
  T ("%*.1s", width, s);
  T ("%*.*s", width, prec, s);
  T ("%*s", width, "123");
  T ("%.*s", prec, "123");
  T ("%1.*s", prec, "123");
  T ("%*.1s", width, "123");
  T ("%*.*s", width, prec, "123");
}


/* { dg-final { scan-tree-dump-times "snprintf" 27 "optimized"} } */
