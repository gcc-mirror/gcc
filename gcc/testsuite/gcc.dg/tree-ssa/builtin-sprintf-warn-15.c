/* PR middle-end/79692 - -Wformat-overflow false positive on an integer
   directive with unknown width
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" }
   { dg-require-effective-target int32plus } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer and objsize macros
   below make use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

void sink (char*, char*);

int dummy_sprintf (char*, const char*, ...);

char buffer [1024];
extern char *ptr;

static int int_range (int min, int max)
{
  extern int int_value (void);
  int n = int_value ();
  return n < min || max < n ? min : n;
}

/* Evaluate to an array of SIZE characters when non-negative, or to
   a pointer to an unknown object otherwise.  */
#define buffer(size)					\
  ((0 <= size) ? buffer + sizeof buffer - (size) : ptr)

/* Helper to expand function to either __builtin_f or dummy_f to
   make debugging GCC easy.  */
#define FUNC(f)							\
  ((!LINE || LINE == __LINE__) ? __builtin_ ## f : dummy_ ## f)

/* Macro to verify that calls to __builtin_sprintf (i.e., with no size
   argument) issue diagnostics by correctly determining the size of
   the destination buffer.  */
#define T(size, ...)						\
  (FUNC (sprintf) (buffer (size),  __VA_ARGS__),		\
   sink (buffer, ptr))

/* Return a signed integer in the range [MIN, MAX].  */
#define R(min, max)  int_range (min, max)

void test_unknown_width_integer (int w, int i)
{
  T (10, "%*d", w, i);
  T (10, "%*d", w, R (0, 12345));

  T (10, "%*i", w, i);
  T (10, "%*i", w, R (0, 12345));

  T (10, "%*o", w, i);
  T (10, "%*o", w, R (0, 12345));

  T (10, "%*i", w, i);
  T (10, "%*i", w, R (0, 12345));
}

void test_unknown_width_floating (int w, double d)
{
  T ( 7, "%*a", w, d);
  T (21, "%*a", w, 3.141);

  T (12, "%*e",  w, d);    /* { dg-warning "may write a terminating nul" } */
  T (12, "%#*e", w, d);    /* { dg-warning "may write a terminating nul" } */
  T (13, "%*e",  w, d);
  T (13, "%#*e", w, d);
  T (13, "%*e",  w, 3.141);

  T ( 8, "%*f",  w, d);   /* { dg-warning "may write a terminating nul" } */
  T ( 8, "%#*f", w, d);   /* { dg-warning "may write a terminating nul" } */
  T ( 9, "%*f",  w, d);
  T ( 9, "%#*f", w, d);
  T ( 9, "%*f",  w, 3.141);
  T ( 9, "%#*f", w, 3.141);

  T (12, "%*g", w, d);   /* { dg-warning "may write a terminating nul" } */
  T (13, "%*g", w, d);
  T (13, "%*g", w, 3.141);
}

void test_unknown_precision_integer (int p, int i, double d)
{
  T (10, "%.*d", p, i);
  T (10, "%.*d", p, R (0, 12345));

  T (10, "%.*i", p, i);
  T (10, "%.*i", p, R (0, 12345));

  T (10, "%.*o", p, i);
  T (10, "%.*o", p, R (0, 12345));

  T (10, "%.*i", p, i);
  T (10, "%.*i", p, R (0, 12345));
}

void test_unknown_precision_floating (int p, double d)
{
  T ( 0, "%.*a", R (-1, 0), d); /* { dg-warning "between 3 and 24 " } */
  T ( 6, "%.*a", R (-1, 0), d); /* { dg-warning "may write a terminating nul" } */
  T ( 7, "%.*a", R (-1, 0), d);
  T ( 7, "%.*a", p, d);
  T (21, "%.*a", p, 3.141);

  T ( 0, "%.*e",  R (-1, 0), d); /* { dg-warning "between 3 and 14 " } */
  T ( 0, "%.*e",  R (-1, 6), d); /* { dg-warning "between 3 and 14 " } */
  T ( 5, "%.*e",  R (-1, 6), d); /* { dg-warning "may write a terminating nul" } */
  T ( 6, "%.*e",  R (-1, 6), d);
  /* "%.0e", 0.0 results in 3 or 5 bytes: "inf"/"nan" or "0e+00"  */
  T ( 5, "%.*e",  p, d);      /* { dg-warning "may write a terminating nul" } */
  /* "%#.0e", 0.0 results in 3 or 6 bytes: "inf"/"nan" or "0.e+00"  */
  T ( 6, "%#.*e", p, d);      /* { dg-warning "may write a terminating nul" } */
  T ( 6, "%.*e",  p, d);
  T ( 6, "%.*e",  p, 3.141);
  T ( 6, "%#.*e", p, 3.141);  /* { dg-warning "writing a terminating nul" } */
  T ( 7, "%#.*e", p, 3.141);

  T ( 0, "%.*f",  R (-1, 0), d); /* { dg-warning "between 1 and 317 " } */
  T ( 0, "%.*f",  R (-1, 6), d); /* { dg-warning "between 1 and 317 " } */
  T ( 3, "%.*f",  R (-1, 6), d); /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%.*f",  R (-1, 6), d);
  /* "%.0f", 0.0 results in 1 byte: "0" but precision of at least 1
     is likely, resulting in "0.0".  */
  T ( 3, "%.*f",  p, d);   /* { dg-warning "may write a terminating nul" } */
  /* "%#.0f", 0.0 results in 2 bytes: "0." but precision of at least 1
     is likely, resulting in "0.0".  */
  T ( 3, "%#.*f", p, d);   /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%.*f",  p, d);
  T ( 4, "%#.*f", p, d);
  T ( 3, "%.*f",  p, 3.141); /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%.*f",  p, 3.141);
  T ( 3, "%#.*f", p, 3.141); /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%#.*f", p, 3.141);

  T ( 0, "%.*g",  R (-1, 0), d); /* { dg-warning "between 1 and 13 " } */
  T (12, "%.*g",  R (-1, 0), d); /* { dg-warning "may write a terminating nul" } */
  T (13, "%.*g",  R (-1, 0), d);
  T (12, "%.*g",  p, d);   /* { dg-warning "may write a terminating nul" } */
  T (12, "%#.*g", p, d);   /* { dg-warning "may write a terminating nul" } */
  T (13, "%.*g",  p, d);
  T (13, "%#.*g", p, d);
  T (12, "%#.*g", R (-1, 0), d);/* { dg-warning "may write a terminating nul" } */
  T (12, "%#.*g", R (-1, 6), d);/* { dg-warning "may write a terminating nul" } */
  T (13, "%#.*g", R (-1, 0), d);
  T ( 6, "%#.*g", R ( 0, 0), d);/* { dg-warning "may write a terminating nul" } */
  T ( 7, "%#.*g", R ( 0, 0), d);
  T ( 6, "%#.*g", R ( 0, 1), d);/* { dg-warning "may write a terminating nul" } */
  T ( 7, "%#.*g", R ( 0, 1), d);
  T ( 3, "%.*g",  p, 3.141); /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%.*g",  p, 3.141);
  T ( 3, "%#.*g", p, 3.141); /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%#.*g", p, 3.141);
}


void test_unknown_width_and_precision_integer (int w, int p, int i)
{
  T (10, "%*.*d", w, p, i);
  T (10, "%*.*d", w, p, R (0, 12345));

  T (10, "%*.*i", w, p, i);
  T (10, "%*.*i", w, p, R (0, 12345));

  T (10, "%*.*o", w, p, i);
  T (10, "%*.*o", w, p, R (0, 12345));

  T (10, "%*.*i", w, p, i);
  T (10, "%*.*i", w, p, R (0, 12345));
}

void test_unknown_width_and_precision_floating (int w, int p, double d)
{
  T ( 7, "%*.*a", w, p, d);
  T (21, "%*.*a", w, p, 3.141);

  /* "%0.0e", 0.0 results in 3 or 5 bytes: "inf"/"nan" or "0e+00"  */
  T ( 5, "%*.*e",  w, p, d);   /* { dg-warning "may write a terminating nul" } */
  /* "%#0.0e", 0.0 results in 3 or 6 bytes: "inf"/"nan" or "0.e+00"  */
  T ( 6, "%#*.*e", w, p, d);   /* { dg-warning "may write a terminating nul" } */
  T ( 6, "%*.*e",  w, p, d);
  T ( 6, "%*.*e",  w, p, 3.141);
  T ( 6, "%#*.*e", w, p, 3.141);/* { dg-warning "writing a terminating nul" } */
  T ( 7, "%#*.*e", w, p, 3.141);

  T ( 3, "%*.*f",  w, p, d);  /* { dg-warning "may write a terminating nul" } */
  T ( 3, "%#*.*f", w, p, d);  /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%*.*f",  w, p, d);
  T ( 4, "%*.*f",  w, p, 3.141);
  T ( 4, "%#*.*f", w, p, 3.141);

  T (13, "%*.*g",  w, p, d);
  T (13, "%#*.*g", w, p, d);
  T (13, "%*.*g",  w, p, 3.141);
  T (13, "%#*.*g", w, p, 3.141);
}
