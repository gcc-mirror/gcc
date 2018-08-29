/* PR tree-optimization/79327 - wrong code at -O2 and -fprintf-return-value
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

char buffer [256];
extern char *ptr;

static int int_range (int min, int max)
{
  extern int int_value (void);
  int n = int_value ();
  return n < min || max < n ? min : n;
}

static unsigned uint_range (unsigned min, unsigned max)
{
  extern unsigned uint_value (void);
  unsigned n = uint_value ();
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

/* Return a unsigned integer in the range [MIN, MAX].  */
#define U(min, max)  uint_range (min, max)

/* Exercise the hh length modifier with ranges.  */
void test_hh (void)
{
  T (0, "%hhi", R (  -1,    0));    /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hhi", R (  -1,    1));    /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hhi", R (  -1,   12));    /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hhi", R (  -1,  123));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhi", R (  -1,  128));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (  -1,  257));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (  -1, 1234));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R ( -12,  -11));    /* { dg-warning "writing 3 bytes" } */
  T (0, "%hhi", R ( -12,   -1));    /* { dg-warning "between 2 and 3 bytes" } */
  T (0, "%hhi", R ( -12,    0));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhi", R ( -12,    1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhi", R ( -12,   12));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhi", R ( -12,  123));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhi", R ( -12,  128));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R ( -12,  257));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R ( -12, 1234));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R ( -99,  -10));    /* { dg-warning "writing 3 bytes" } */
  T (0, "%hhi", R (-123,   -1));    /* { dg-warning "between 2 and 4 bytes" } */
  T (0, "%hhi", R (-123,    0));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-123,    1));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-123,   12));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-123,  123));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-123,  257));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-123, 1234));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-129,    1));    /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hhi", R (-130, -129));    /* { dg-warning "writing 3 bytes" } */

  T (0, "%hhi", U (   0,  127));    /* { dg-warning "between 1 and 3 bytes" } */

  /* The following results in either "127" and "-128" so the ideal result
     should be "between 3 and 4 bytes" but because of the overflow from
     128 to -128 in the %hhi directive the input range is reset to that
     of char, or [CHAR_MIN, CHAR_MAX], and the warning reflects that.  */
  T (0, "%hhi", U ( 127,  128));    /* { dg-warning "between \[13\] and 4 bytes" } */
  /* The following results in either "-128" or "-127".  */
  T (0, "%hhi", U ( 128,  129));    /* { dg-warning "writing 4 bytes" } */
  /* The following results in between "-128" and "-99".  */
  T (0, "%hhi", U ( 128,  157));    /* { dg-warning "writing between 3 and 4 bytes" } */
  /* Between "-128" and "-1".  */
  T (0, "%hhi", U ( 128,  255));    /* { dg-warning "writing between 2 and 4 bytes" } */
  /* Between "-128" and "0".  */
  T (0, "%hhi", U ( 128,  256));    /* { dg-warning "writing between 1 and 4 bytes" } */
  /* Between "-128" and "" (zero formats as nothing with zero precision).  */
  T (0, "%.0hhi", U ( 128,  256));  /* { dg-warning "writing up to 4 bytes" } */
  /* Same as above but with a range of precisions including zero.  */
  T (0, "%.*hhi",                   /* { dg-warning "writing up to 4 bytes" } */
     R (0, 1), U ( 128,  256));
  /* Same as above but with a positive range of precisions.  */
  T (0, "%.*hhi",                   /* { dg-warning "between 1 and 4 bytes" } */
     R (1, 2), U ( 128,  256));
  /* Precision range includes zero but width is non-zero so output cannot
     be empty.  */
  T (0, "%1.*hhi",                  /* { dg-warning "between 1 and 4 bytes" } */
     R (0, 2), U ( 128,  256));
  /* Same as above but with a width range.  */
  T (0, "%*.*hhi",                  /* { dg-warning "between 1 and 4 bytes" } */
     R (1, 2), R (0, 2), U ( 128,  256));
  /* Same as above but this time width range does include zero.  */
  T (0, "%*.*hhi",                  /* { dg-warning "up to 4 bytes" } */
     R (0, 2), R (0, 2), U ( 128,  256));

  /* Range of precisions in excess of the number of digits and sign.  */
  T (0, "%.*hhi",                   /* { dg-warning "between 5 and 8 bytes" } */
     R (5, 7), U ( 128,  256));

  /* Between "-128" and "+0".  */
  T (0, "%+hhi",  U ( 128,  256));  /* { dg-warning "between 2 and 4 bytes" } */
  /* Between "-128" and " 0".  */
  T (0, "% hhi",  U ( 128,  256));  /* { dg-warning "between 2 and 4 bytes" } */

  T (0, "%hhu", R (  -1,    1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (  -1,   12));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (  -1,  123));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (  -1,  128));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (  -1,  257));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (  -1, 1234));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12,    1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12,   12));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12,  123));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12,  128));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12,  257));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R ( -12, 1234));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-123,    1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-123,   12));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-123,  123));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-123,  257));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-123, 1234));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-129,    1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hhu", R (-199, -159));    /* { dg-warning "writing 2 bytes" } */
  T (0, "%hhu", R (-255, -250));    /* { dg-warning "writing 1 byte" } */
}

/* Exercise the h length modifier.  */
void test_h (void)
{
  T (0, "%hi", R (    -1,     0));  /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hi", R (     0,     1));  /* { dg-warning "writing 1 byte" } */
  T (0, "%hi", R (    -1,     1));  /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hi", R (    -1,    12));  /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%hi", R (   -12,     1));  /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%hi", R (   -99,   -10));  /* { dg-warning "writing 3 bytes" } */
  T (0, "%hi", R (  -123,     4));  /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%hi", R ( -1234,    56));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hi", R ( -1234,   567));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hi", R ( -1234,  5678));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%ho", R (-32768,-32767));  /* { dg-warning "writing 6 bytes" } */

  T (0, "%ho", R (    -1,     0));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R (     0,     1));  /* { dg-warning "writing 1 byte" } */
  T (0, "%ho", R (    -1,     1));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R (    -1,    12));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R (   -12,     1));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R (  -123,     4));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R ( -1234,    56));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R ( -1234,   567));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R ( -1234,  5678));  /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%ho", R (-32768,-32767));  /* { dg-warning "writing 6 bytes" } */

  T (0, "%hu", R (    -1,     0));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R (     0,     1));  /* { dg-warning "writing 1 byte" } */
  T (0, "%hu", R (    -1,     1));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R (    -1,    12));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R (   -12,     1));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R (  -123,     4));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R ( -1234,    56));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R ( -1234,   567));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R ( -1234,  5678));  /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%hu", R (-32768,-32767));  /* { dg-warning "writing 5 bytes" } */

  T (0, "%hx", R (-32768,-32767));  /* { dg-warning "writing 4 bytes" } */
}

/* Exercise integer directives with no length modifier.  */
void test_diou (void)
{
  T (0, "%d", R (    -1,     0));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%i", R (     0,     1));   /* { dg-warning "writing 1 byte" } */
  T (0, "%d", R (    -1,     1));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%i", R (    -1,    12));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%d", R (   -12,     1));   /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%i", R (  -123,     4));   /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%d", R ( -1234,    56));   /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%i", R ( -1234,   567));   /* { dg-warning "between 1 and 5 bytes" } */
  T (0, "%d", R ( -1234,  5678));   /* { dg-warning "between 1 and 5 bytes" } */

  T (0, "%u", R (    -1,     0));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R (     0,     1));  /* { dg-warning "writing 1 byte" } */
  T (0, "%u", R (    -1,     1));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R (    -1,    12));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R (   -12,     1));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R (  -123,     4));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R ( -1234,    56));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R ( -1234,   567));  /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%u", R ( -1234,  5678));  /* { dg-warning "between 1 and 10 bytes" } */

  T (0, "%o", R (    -1,     0));  /* { dg-warning "between 1 and 11 bytes" } */
  T (0, "%o", R (    -2,     1));  /* { dg-warning "between 1 and 11 bytes" } */
  T (0, "%o", R (     0,     1));  /* { dg-warning "writing 1 byte" } */

  T (0, "%x", R (    -1,     0));  /* { dg-warning "between 1 and 8 bytes" } */
  T (0, "%x", R (    -2,     1));  /* { dg-warning "between 1 and 8 bytes" } */
  T (0, "%x", R (     0,     1));  /* { dg-warning "writing 1 byte" } */
}
