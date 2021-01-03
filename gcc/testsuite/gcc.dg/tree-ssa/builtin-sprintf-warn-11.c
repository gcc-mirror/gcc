/* PR middle-end/79275 - -Wformat-overflow false positive exceeding INT_MAX
   in glibc sysdeps/posix/tempname.c
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" } */

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

extern int int_value (void);
extern size_t size_value (void);

static int int_range (int min, int max)
{
  int n = int_value ();
  return n < min || max < n ? min : n;
}

void sink (char*, char*);

int dummy_sprintf (char*, const char*, ...);

char buffer [256];
extern char *ptr;

const char s0[] = "";
const char s1[] = "1";
const char s2[] = "12";
const char s3[] = "123";
const char s4[] = "1234";
const char s5[] = "12345";
const char s6[] = "123456";
const char s7[] = "1234567";
const char s8[] = "12345678";
const char s9[] = "123456789";
extern const char sx[];
extern const char sy[];

/* Wide string literals outside the ASCII range to avoid assumptions
   about the number of narrow characters they might convert to beyond
   up to 6 bytes each (the maximum for UTF-8 not exceeded by any known
   encoding).  */
const wchar_t ws0[] = L"";
const wchar_t ws1[] = L"\u1111";
const wchar_t ws2[] = L"\u1111\u2222";
const wchar_t ws3[] = L"\u1111\u2222\u3333";
const wchar_t ws4[] = L"\u1111\u2222\u3333\u4444";
const wchar_t ws5[] = L"\u1111\u2222\u3333\u4444\u5555";
const wchar_t ws6[] = L"\u1111\u2222\u3333\u4444\u5555\u6666";
const wchar_t ws7[] = L"\u1111\u2222\u3333\u4444\u5555\u6666\u7777";
const wchar_t ws8[] =
  L"\u1111\u2222\u3333\u4444\u5555\u6666\u7777\u8888";
const wchar_t ws9[] =
  L"\u1111\u2222\u3333\u4444\u5555\u6666\u7777\u8888\u9999";
extern const wchar_t wsx[];
extern const wchar_t wsy[];

static const int imin = INT_MIN;
static const int imax = INT_MAX;

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

/* Return a value in the range [MIN, MAX].  */
#define IR(min, max)  int_range (min, max)

/* Return a string whose length is in the range [MIN, MAX] where
   both MIN and MAX must be digits in the range [0, 9].  */
#define SR(min, max)  (int_value () < 0 ? s##min : s##max)

/* Return a wide string whose length is in the range [MIN, MAX] where
   both MIN and MAX must be digits in the range [0, 9].  */
#define WR(min, max)  (int_value () < 0 ? ws##min : ws##max)

void test_narrow_string_with_precision (void)
{
  T (-1, "%.*s", IR ( 0,  1), SR (0, 1));
  T (-1, "%.*s", IR ( 0,  1), SR (0, 2));
  T (-1, "%.*s", IR ( 0,  1), SR (0, 3));
  T (-1, "%.*s", IR ( 0,  1), SR (0, 4));
  T (-1, "%.*s", IR ( 0,  1), SR (0, 9));
  T (-1, "%.*s", IR ( 0,  2), SR (0, 9));
  T (-1, "%.*s", IR ( 0,  3), SR (0, 9));
  T (-1, "%.*s", IR ( 0,  4), SR (0, 9));
  T (-1, "%.*s", IR ( 0,  9), SR (0, 9));
  T (-1, "%.*s", IR ( 0, 99), SR (0, 9));
  T (-1, "%.*s", IR ( 0, 99), SR (0, x));
  T (-1, "%.*s", IR ( 0, 99), SR (1, x));
  T (-1, "%.*s", IR ( 0, 99), SR (x, 1));
  T (-1, "%.*s", IR ( 0, 99), SR (x, 9));

  T (-1, "%.*s", IR (imax / 3, imax / 2), SR (x, y));

  /* Non-constant zero length string.  */
  T ( 0, "%.*s", IR (imin, -1), SR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (imin,  0), SR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-1,    0), SR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-1,    1), SR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-1,   99), SR (0, 0));   /* { dg-warning "writing a terminating nul" } */

  /* String with length between 0 and 1 character.  */
  T ( 0, "%.*s", IR (imin, -1), SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR (imin, 0),  SR (0, 1));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-2, -1),   SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR (-2,  0),   SR (0, 1));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR ( 0,  1),   SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0,  2),   SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0, 99),   SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0, imax), SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 1, imax), SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 9, imax), SR (0, 1));   /* { dg-warning "writing up to 1 byte" } */

  /* String with length between 2 and 3 characters.  */
  T ( 0, "%.*s", IR (imin, -1), SR (2, 3));   /* { dg-warning "writing between 2 and 3 bytes" } */
  T ( 0, "%.*s", IR (imin, 0),  SR (2, 3));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-2, -1),   SR (2, 3));   /* { dg-warning "writing between 2 and 3 bytes" } */
  T ( 0, "%.*s", IR (-2,  0),   SR (2, 3));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (-2,  1),   SR (2, 3));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0,  1),   SR (2, 3));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0,  2),   SR (2, 3));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*s", IR ( 0, 99),   SR (2, 3));   /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%.*s", IR ( 0, imax), SR (2, 3));   /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%.*s", IR ( 1, 99),   SR (2, 3));   /* { dg-warning "writing between 1 and 3 bytes" } */
  T ( 0, "%.*s", IR ( 9, 99),   SR (2, 3));   /* { dg-warning "writing between 2 and 3 bytes" } */

  T ( 0, "%.*s", IR ( 0,  1),   SR (0, 9));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*s", IR ( 0,  2),   SR (0, 9));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*s", IR ( 0,  9),   SR (0, 9));   /* { dg-warning "writing up to 9 bytes" } */
  T ( 0, "%.*s", IR ( 0, 79),   SR (0, 9));   /* { dg-warning "writing up to 9 bytes" } */
  T ( 0, "%.*s", IR ( 1,  2),   SR (0, 9));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*s", IR ( 2,  3),   SR (0, 9));   /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%.*s", IR ( 7, 13),   SR (0, 9));   /* { dg-warning "writing up to 9 bytes" } */

  /* String between N and unknown number of characters long.  */
  T ( 0, "%.*s", IR (imin, -1), SR (0, x));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*s", IR (imin, -1), SR (1, x));   /* { dg-warning "writing likely 1 or more bytes" } */
  T ( 1, "%.*s", IR (imin, -1), SR (1, x));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (imin, -1), SR (8, x));   /* { dg-warning "writing likely 8 or more bytes" } */
  T ( 1, "%.*s", IR (imin, -1), SR (x, 9));   /* { dg-warning "writing likely 9 or more bytes" } */

  /* Unknown strings.  */
  T ( 1, "%.*s", IR (imin, -1), SR (x, y));
  T ( 1, "%.*s", IR (imin,  0), SR (x, y));
  T ( 1, "%.*s", IR ( -99,  1), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (  -2,  2), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (  -1, 99), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (   0, 99), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (   1, 99), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*s", IR (   9, 99), SR (x, y));   /* { dg-warning "may write a terminating nul" } */
}

void test_narrow_string_with_width_and_precision (void)
{
  T (-1, "%*.*s", IR ( 0,  1), IR ( 0,  1), SR (0, 1));
  T (-1, "%*.*s", IR ( 0,  1), IR ( 0,  1), SR (0, 2));
  T (-1, "%*.*s", IR ( 0,  1), IR ( 0,  1), SR (0, 3));
  T (-1, "%*.*s", IR ( 0,  1), IR ( 0,  1), SR (0, 4));
  T (-1, "%*.*s", IR ( 0,  1), IR ( 0,  1), SR (0, 9));
  T (-1, "%*.*s", IR ( 0,  2), IR ( 0,  2), SR (0, 9));
  T (-1, "%*.*s", IR ( 0,  3), IR ( 0,  3), SR (0, 9));
  T (-1, "%*.*s", IR ( 0,  4), IR ( 0,  4), SR (0, 9));
  T (-1, "%*.*s", IR ( 0,  9), IR ( 0,  9), SR (0, 9));
  T (-1, "%*.*s", IR ( 0, 99), IR ( 0, 99), SR (0, 9));
  T (-1, "%*.*s", IR ( 0, 99), IR ( 0, 99), SR (0, x));
  T (-1, "%*.*s", IR ( 0, 99), IR ( 0, 99), SR (1, x));
  T (-1, "%*.*s", IR ( 0, 99), IR ( 0, 99), SR (x, 1));
  T (-1, "%*.*s", IR ( 0, 99), IR ( 0, 99), SR (x, 9));
  T (-1, "%*.*s", IR (12, 34), IR (45, 67), SR (x, 9));
  T (-1, "%*.*s", IR (12, 34), IR (45, 67), SR (x, y));

  T (-1, "%*.*s", IR (imax / 5, imax / 4), IR (imax / 3, imax / 2), SR (x, y));

  T (-1, "%*.*s %*.*s",
     IR (imax / 9, imax / 8), IR (imax / 7, imax / 6), SR (x, y),
     IR (imax / 5, imax / 4), IR (imax / 3, imax / 2), SR (x, y));

  /* The two directives below combined convert to [INT_MAX -1, INT_MAX + 1].
     Since the lower end of the range doesn't exceed INT_MAX no warning
     is expected.  */
  T (-1, "%*.*s%*.*s",
     IR (imax - 6, imax - 3), IR (1, 2), SR (x, y),
     IR (       5,        6), IR (3, 4), SR (x, y));

  /* The three directives below (the two %s plus the space in between)
     combined convert to [INT_MAX + 1, INT_MAX + 2].  Since the lower
     end of the range exceeds INT_MAX a warning is expected.  In ILP32,
     the output overflows the maximum object size.  */
  T (-1, "%*.*s %*.*s",
     /* { dg-warning "INT_MAX" "LP64" { target lp64 } .-1 }
	{ dg-warning "directive writing between 5 and 6 bytes into a region of size between 2 and 4" "ILP32" { target ilp32 } .-2 }
      */
     IR (imax - 5, imax - 3), IR (1, 2), SR (x, y),
     IR (       5,        6), IR (3, 4), SR (x, y));

   /* Non-constant zero length string.  */
  T ( 0, "%*.*s", IR ( 0, 1), IR (imin, -1), SR (0, 0));    /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%*.*s", IR ( 0, 2), IR (imin, -1), SR (0, 0));    /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (imin, -1), SR (0, 0));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  1), SR (0, 0));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  1), SR (0, 1));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  2), SR (0, 1));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  3), SR (0, 1));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  1), SR (3, 5));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  2), SR (3, 5));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  3), SR (3, 5));    /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  4), SR (3, 5));    /* { dg-warning "writing up to 4 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  5), SR (3, 5));    /* { dg-warning "writing up to 5 bytes" } */
  T ( 0, "%*.*s", IR ( 0, 3), IR (   0,  6), SR (3, 5));    /* { dg-warning "writing up to 5 bytes" } */

  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  1), SR (3, 5));    /* { dg-warning "writing between 1 and 2 bytes" } */
  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  2), SR (3, 5));    /* { dg-warning "writing between 1 and 2 bytes" } */
  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  3), SR (3, 5));    /* { dg-warning "writing between 1 and 3 bytes" } */
  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  4), SR (3, 5));    /* { dg-warning "writing between 1 and 4 bytes" } */
  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  5), SR (3, 5));    /* { dg-warning "writing between 1 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 1, 2), IR (   0,  6), SR (3, 5));    /* { dg-warning "writing between 1 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   0,  6), SR (3, 5));    /* { dg-warning "writing between 2 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   1,  6), SR (3, 5));    /* { dg-warning "writing between 2 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   2,  6), SR (3, 5));    /* { dg-warning "writing between 2 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   3,  6), SR (3, 5));    /* { dg-warning "writing between 3 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   4,  6), SR (3, 5));    /* { dg-warning "writing between 3 and 5 bytes" } */
  T ( 0, "%*.*s", IR ( 2, 3), IR (   5,  6), SR (3, 5));    /* { dg-warning "writing between 3 and 5 bytes" } */
}

void test_wide_string (void)
{
  T (-1, "%.*ls", IR ( 0,  1), WR (0, 1));
  T (-1, "%.*ls", IR ( 0,  1), WR (0, 2));
  T (-1, "%.*ls", IR ( 0,  1), WR (0, 3));
  T (-1, "%.*ls", IR ( 0,  1), WR (0, 4));
  T (-1, "%.*ls", IR ( 0,  1), WR (0, 9));
  T (-1, "%.*ls", IR ( 0,  2), WR (0, 9));
  T (-1, "%.*ls", IR ( 0,  3), WR (0, 9));
  T (-1, "%.*ls", IR ( 0,  4), WR (0, 9));
  T (-1, "%.*ls", IR ( 0,  9), WR (0, 9));
  T (-1, "%.*ls", IR ( 0, 99), WR (0, 9));
  T (-1, "%.*ls", IR ( 0, 99), WR (0, x));
  T (-1, "%.*ls", IR ( 0, 99), WR (1, x));
  T (-1, "%.*ls", IR ( 0, 99), WR (x, 1));
  T (-1, "%.*ls", IR ( 0, 99), WR (x, 9));

   /* Non-constant zero length string.  */
  T ( 0, "%.*ls", IR (imin, -1), WR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (imin,  0), WR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-1,    0), WR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-1,    1), WR (0, 0));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-1,   99), WR (0, 0));   /* { dg-warning "writing a terminating nul" } */

  /* String with length between 0 and 1 character.  */
  T ( 0, "%.*ls", IR (imin, -1), WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */
  T ( 0, "%.*ls", IR (imin, 0),  WR (0, 1));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-2, -1),   WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */
  T ( 0, "%.*ls", IR (-2,  0),   WR (0, 1));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR ( 0,  1),   WR (0, 1));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*ls", IR ( 0,  2),   WR (0, 1));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*ls", IR ( 0, 99),   WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */
  T ( 0, "%.*ls", IR ( 0, imax), WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */
  T ( 0, "%.*ls", IR ( 1, imax), WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */
  T ( 0, "%.*ls", IR ( 9, imax), WR (0, 1));   /* { dg-warning "writing up to 6 bytes" } */

  /* String with length between 2 and 3 characters.  */
  T ( 0, "%.*ls", IR (imin, -1), WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */
  T ( 0, "%.*ls", IR (imin, 0),  WR (2, 3));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-2, -1),   WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */
  T ( 0, "%.*ls", IR (-2,  0),   WR (2, 3));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (-2,  1),   WR (2, 3));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*ls", IR ( 0,  1),   WR (2, 3));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*ls", IR ( 0,  2),   WR (2, 3));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*ls", IR ( 0, 99),   WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */
  T ( 0, "%.*ls", IR ( 0, imax), WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */
  T ( 0, "%.*ls", IR ( 1, 99),   WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */
  T ( 0, "%.*ls", IR ( 9, 99),   WR (2, 3));   /* { dg-warning "writing up to 18 bytes" } */

  T ( 0, "%.*ls", IR ( 0,  1),   WR (0, 9));   /* { dg-warning "writing up to 1 byte" } */
  T ( 0, "%.*ls", IR ( 0,  2),   WR (0, 9));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*ls", IR ( 0,  9),   WR (0, 9));   /* { dg-warning "writing up to 9 bytes" } */
  T ( 0, "%.*ls", IR ( 0, 53),   WR (0, 9));   /* { dg-warning "writing up to 53 bytes" } */
  T ( 0, "%.*ls", IR ( 0, 55),   WR (0, 9));   /* { dg-warning "writing up to 54 bytes" } */
  T ( 0, "%.*ls", IR ( 1,  2),   WR (0, 9));   /* { dg-warning "writing up to 2 bytes" } */
  T ( 0, "%.*ls", IR ( 2,  3),   WR (0, 9));   /* { dg-warning "writing up to 3 bytes" } */
  T ( 0, "%.*ls", IR ( 7, 13),   WR (0, 9));   /* { dg-warning "writing up to 13 bytes" } */

  /* String between N and unknown number of characters long.  */
  T ( 0, "%.*ls", IR (imin, -1), WR (0, x));   /* { dg-warning "writing a terminating nul" } */
  T ( 0, "%.*ls", IR (imin, -1), WR (1, x));   /* { dg-warning "writing likely 2 or more bytes" } */
  T ( 1, "%.*ls", IR (imin, -1), WR (1, x));   /* { dg-warning "writing likely 2 or more bytes" } */
  T ( 1, "%.*ls", IR (imin, -1), WR (8, x));   /* { dg-warning "writing likely 16 or more bytes" } */
  T ( 1, "%.*ls", IR (imin, -1), WR (x, 9));   /* { dg-warning "writing likely 18 or more bytes" } */

  /* Unknown strings.  */
  T ( 1, "%.*ls", IR (imin, -1), WR (x, y));
  T ( 1, "%.*ls", IR (imin,  0), WR (x, y));
  T ( 1, "%.*ls", IR ( -99,  1), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*ls", IR (  -2,  2), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*ls", IR (  -1, 99), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*ls", IR (   0, 99), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*ls", IR (   1, 99), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
  T ( 1, "%.*ls", IR (   9, 99), WR (x, y));   /* { dg-warning "may write a terminating nul" } */
}
