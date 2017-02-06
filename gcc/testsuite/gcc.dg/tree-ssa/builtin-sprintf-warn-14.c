/* PR middle-end/79376 - wrong lower bound with %s and non-constant
   strings in -Wformat-overflow
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__  size_t;

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer and objsize macros
   below make use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

extern int int_value (void);

int int_range (int min, int max)
{
  int n = int_value ();
  return n < min || max < n ? min : n;
}

const char*
choose_string (const char *s1, const char *s2, const char *s3)
{
  int i = int_value ();
  return i < 0 ? s1 : 0 < i ? s3 : s2;
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
#define R(min, max)  int_range (min, max)

/* Return one of the strings S1, S2, S3.  */
#define S(s1, s2, s3) choose_string (s1, s2, s3)

struct S {
  char a1[1];
  char a2[2];
  char a3[3];
  char a4[4];
  char a5[5];
  char ax[];
};

void test_strings (struct S *s)
{
  T (0, "%-s", S (s->a1, s->a1, s1));   /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s->a1, s->a2, s1));   /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s->a2, s->a1, s1));   /* { dg-warning "writing up to 1 byte" } */

  T (0, "%-s", S (s->a1, s1, s->a1));   /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s->a1, s1, s->a2));   /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s->a2, s1, s->a1));   /* { dg-warning "writing up to 1 byte" } */

  T (0, "%-s", S (s1, s->a1, s1));      /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s1, s->a1, s1));      /* { dg-warning "writing up to 1 byte" } */
  T (0, "%-s", S (s1, s->a2, s1));      /* { dg-warning "writing up to 1 byte" } */

  T (0, "%-s", S (s->a1, s->a1, s2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a1, s->a2, s2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a2, s->a1, s2));   /* { dg-warning "writing up to 2 bytes" } */

  T (0, "%-s", S (s->a1, s2, s->a1));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a1, s2, s->a2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a2, s2, s->a1));   /* { dg-warning "writing up to 2 bytes" } */

  T (0, "%-s", S (s2, s->a1, s1));      /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s2, s->a1, s1));      /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s2, s->a2, s1));      /* { dg-warning "writing up to 2 bytes" } */

  T (0, "%-s", S (s->a1, s->a1, s2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a1, s->a2, s2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a2, s->a1, s2));   /* { dg-warning "writing up to 2 bytes" } */

  T (0, "%-s", S (s->a1, s2, s->a1));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a1, s2, s->a2));   /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s->a2, s2, s->a1));   /* { dg-warning "writing up to 2 bytes" } */

  T (0, "%-s", S (s2, s->a1, s1));      /* { dg-warning "writing up to 2 bytes" } */
  T (0, "%-s", S (s3, s->a1, s2));      /* { dg-warning "writing up to 3 bytes" } */
  T (0, "%-s", S (s4, s->a2, s3));      /* { dg-warning "writing up to 4 bytes" } */

  T (0, "%-s", S (s->a3, s->a5, s3));   /* { dg-warning "writing up to 4 bytes" } */
  T (0, "%-s", S (s->a3, s->a5, s3));   /* { dg-warning "writing up to 4 bytes" } */
  T (0, "%-s", S (s->a3, s->a5, s3));   /* { dg-warning "writing up to 4 bytes" } */

  T (0, "%-s", S (s->a3, s3, s->a5));   /* { dg-warning "writing up to 4 bytes" } */
  T (0, "%-s", S (s->a5, s3, s->a3));   /* { dg-warning "writing up to 4 bytes" } */
  T (0, "%-s", S (s->a3, s3, s->a5));   /* { dg-warning "writing up to 4 bytes" } */

  T (0, "%-s", S (s3, s->a1, s4));      /* { dg-warning "writing up to 4 bytes" } */
  T (0, "%-s", S (s4, s->a2, s3));      /* { dg-warning "writing up to 4 bytes" } */
}

void test_strings_with_width (struct S *s)
{
  T (0, "%*s",                          /* { dg-warning "writing up to 4 bytes" } */
     R (0, 1),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing up to 4 bytes" } */
     R (0, 4),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing between 1 and 4 bytes" } */
     R (1, 4),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing between 2 and 4 bytes" } */
     R (2, 4),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing between 3 and 4 bytes" } */
     R (3, 4),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing between 3 and 5 bytes" } */
     R (3, 5),
     S (s->a3, s->a5, s3));

  T (0, "%*s",                          /* { dg-warning "writing between 3 and 9 bytes" } */
     R (3, 9),
     S (s->a3, s->a5, s3));

  T (3, "%*s",                          /* { dg-warning "writing between 3 and 9 bytes" } */
     R (3, 9),
     S (s->a3, s->a5, s3));

  /* The longest string fits but the terminating nul will overflow.  Since
     the null won't overflow with the shorter strings the warning is a "may
     write."  */
  T (4, "%*s",                          /* { dg-warning "may write a terminating nul" } */
     R (3, 9),
     S (s->a3, s->a5, s3));
}

void test_strings_with_width_and_precisin (struct S *s)
{
  T (0, "%*.*s",                        /* { dg-warning "writing up to 1 byte" } */
     R (0, 1),
     R (0, 1),
     S (s->a3, s->a5, s3));

  T (0, "%*.*s",                        /* { dg-warning "writing up to 2 bytes" } */
     R (0, 2),
     R (0, 1),
     S (s->a3, s->a5, s3));

  T (0, "%*.*s",                        /* { dg-warning "writing up to 9 bytes" } */
     R (0, 9),
     R (0, 1),
     S (s->a3, s->a5, s3));

  /* Since the longest string/array fits there is no warning even if
     the maximum width would cause overflow.  */
  T (6, "%*.*s",
     R (0, 9),
     R (0, 1),
     S (s->a3, s->a5, s3));

  T (0, "%*.*s",                        /* { dg-warning "writing up to 2 bytes" } */
     R (0, 2),
     R (0, 2),
     S (s->a3, s->a5, s3));

  T (0, "%*.*s",                        /* { dg-warning "writing up to 3 bytes" } */
     R (0, 3),
     R (0, 2),
     S (s->a3, s->a5, s3));
}
