/* PR middle-end/78476 - snprintf(0, 0, ...) with known arguments not
   optimized away
   PR middle-end/78512 - r242674 miscompiles Linux kernel
   A negative test complementing builtin-sprintf-5.c to verify that calls
   to the function that do not return a constant are not optimized away.
   Test also verifies that unknown directives prevent the optimization.
   { dg-do compile }
   { dg-options "-O2 -Wformat -fdump-tree-optimized" }
   { dg-require-effective-target int32plus } */

typedef __SIZE_TYPE__ size_t;

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

  /* Each of the bounds of the ranges below results in just one byte
     on output because they convert to the value 9 in type char, yet
     other values in those ranges can result in up to four bytes.
     For example, 4240 converts to -112.  Verify that the output
     isn't folded into a constant.  This assumes __CHAR_BIT__ of 8.  */
  T ("%hhi", R (4104, 4360) + 1);
  T ("%hhu", R (4104, 4360) + 1);

  /* Here, the range includes all possible lengths of output for
     a 16-bit short and 32-bit int.  */
  T ("%hi", R (65536, 65536 * 2));
  T ("%hu", R (65536, 65536 * 2));

  T ("%'i", 1234567);

  for (i = -n; i != n; ++i)
    T ("%*x", n, i);
}

/* Support for %p was removed in response to PR middle-end/78512 due
   to the Linux kernel relying on GCC builtins while at the same time
   providing a large number of extensions to the %p directive that
   interfere with the optimization.  Verify that %p disables it.  */

void test_arg_ptr (int width, int prec, int i)
{
  T ("%p", (void*)0);
  T ("p=%p", (void*)0);
  T ("%s=%p", "p=", (void*)0);
  T ("%i%p", 123, (void*)0);
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

void test_invalid_directive (void)
{
  T ("%");        /* { dg-warning "spurious trailing .%." } */
  T ("abc%");     /* { dg-warning "spurious trailing .%." } */

  T ("%2$i");     /* { dg-warning "operand number out of range" } */
  T ("abc%2$i");  /* { dg-warning "operand number out of range" } */

  T ("%=i", 0);   /* { dg-warning "unknown conversion type character .=." } */
  /* { dg-warning "too many arguments" "" { target *-*-* } .-1 } */

  T ("%*i", "", 0); /* { dg-warning "field width specifier .\\*. expects argument of type .int." } */
  T ("%.*i", "", 0); /* { dg-warning "field precision specifier .\\.\\*. expects argument of type .int." } */
  T ("%.*.i", 0);   /* { dg-warning "unknown conversion type character .\\.." } */
  T ("%Q");       /* { dg-warning "unknown conversion type character .Q." } */
  T ("abc%Q");    /* { dg-warning "unknown conversion type character .Q." } */
}


/* Use 'grep "^ *T (" builtin-sprintf-6.c  | wc -l' to determine
   the count for the directive below.
   { dg-final { scan-tree-dump-times "snprintf" 46 "optimized"} } */
