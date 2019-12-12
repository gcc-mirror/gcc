/* PR middle-end/79448 - unhelpful -Wformat-truncation=2 warning
   { dg-do compile }
   { dg-options "-O2 -Wformat -Wformat-truncation=2 -ftrack-macro-expansion=0" }
   { dg-require-effective-target ptr32plus } */

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

int int_range (int min, int max)
{
  int n = int_value ();
  return n < min || max < n ? min : n;
}

void sink (int, char*, char*);

int dummy_snprintf (char*, size_t, const char*, ...);

char fixed_buffer [256];
extern char *unknown_buffer;
extern size_t unknown_size;

/* Helper to expand function to either __builtin_f or dummy_f to
   make debugging GCC easy.  */
#define FUNC(f)							\
  ((!LINE || LINE == __LINE__) ? __builtin_ ## f : dummy_ ## f)

/* Helper test macro.  */
#define T(size, ...)					\
  do {							\
    size_t n = size < 0 ? unknown_size : size;		\
    char *buf = size < 0 ? unknown_buffer		\
      : n < sizeof fixed_buffer				\
      ? fixed_buffer + sizeof fixed_buffer - size	\
      : unknown_buffer;					\
    FUNC (snprintf) (buf, n, __VA_ARGS__);		\
    sink (0, fixed_buffer, unknown_buffer);		\
  } while (0)

/* Return a value in the range [MIN, MAX].  */
#define IR(min, max)  int_range (min, max)

struct Arrays
{
  char a1[1];
  char a4k[4096];
  char a4kp1[4097];
#if INT_MAX < LONG_MAX
  char amax[INT_MAX];
#else
  char amax[32767];
#endif
  char ax[];
};

void test_string_unchecked (const char *s, const struct Arrays *ar)
{
  /* Verify there is no warning with strings of unknown length.  */
  T (-1, "%-s", s);
  T (-1, "%-s", ar->ax);

  T (-1, "%s%s", s, s);
  T (-1, "%s%s", "", s);
  T (-1, "%s%s", s, "1");
  T (-1, "%s%s", "1", s);

  /* Verify there is no warning with strings of length that cannot
     exceed 4k (because of the array size).  */
  T (-1, "%-s", ar->a1);
  T (-1, "%-s", ar->a4k);

  /* Verify there's no "exceeds minimum required size of 4095" warning
     with multiple %s directives and a combination of strings of unknown
     (and potentially unbounded) length and strings whose length is
     bounded by the size of the arrays they are stored in.  */
  T (-1, "%s%s", s, ar->a4k);
  T (-1, "%s%s", ar->a4k, s);
  T (-1, "%s%s", ar->a4k, ar->a4k);
  T (-1, "%s%s", ar->a4k, "123");
  T (-1, "%s%s", "123", ar->a4k);
  T (-1, "%s%s", ar->ax, ar->a4k);
  T (-1, "%s%s", ar->a4k, ar->ax);

  /* Verify that an array that fits a string longer than 4095 bytes
     does trigger a warning.  */
  T (-1, "%-s", ar->a4kp1);   /* { dg-warning "directive output between 0 and 4096 bytes may exceed minimum required size of 4095" } */

  /* Also verify that a %s directive with width greater than 4095
     triggers a warning even if the argument is not longer than 4k.  */
  T (-1, "%*s", 4096, ar->a4k);   /* { dg-warning "directive output of 4096 bytes exceeds minimum required size of 4095" } */

  /* Verify that precision constrains the putput and suppresses the 4k
     warning.  */
  T (-1, "%.*s", 4095, ar->a4kp1);

  T (-1, "%s %s", s, "");
  T (-1, "%s %s", "", s);
  T (-1, "%s %s", s, "1");
  T (-1, "%s %s", "1", s);

  T (-1, "%s%s%s", s, "1", s);
  T (-1, "%s%s%s", "1", s, "1");
  T (-1, "%s%s%s", s, s, s);
  T (-1, "%*s%*s%*s", 4093, s, 4094, s, 4095, s);
  T (-1, "%s %s %s", s, s, s);
  T (-1, "%s %s %s", ar->a4k, ar->a4k, ar->a4k);
  T (-1, "%s %s %s", ar->ax, ar->ax, ar->ax);

  /* Verify that an array of INT_MAX elements doesn't trigger the INT_MAX
     warning (LP64 only).  */
  T (-1, "%-s", ar->amax);   /* { dg-warning "directive output between 0 and \[0-9\]+ bytes may exceed minimum required size of 4095" } */
}

#undef T
/* Helper test macro.  */
#define T(size, ...)					\
  do {							\
    size_t n = size < 0 ? unknown_size : size;		\
    char *buf = size < 0 ? unknown_buffer		\
      : n < sizeof fixed_buffer				\
      ? fixed_buffer + sizeof fixed_buffer - size	\
      : unknown_buffer;					\
    int r = FUNC (snprintf) (buf, n, __VA_ARGS__);	\
    sink (r, fixed_buffer, unknown_buffer);		\
  } while (0)

void test_string_checked (const char *s, const struct Arrays *ar)
{
  /* Verify there is no warning with strings of unknown length.  */
  T (-1, "%-s", s);
  T (-1, "%-s", ar->ax);

  T (-1, "%s%s", s, s);
  T (-1, "%s%s", "", s);
  T (-1, "%s%s", s, "1");
  T (-1, "%s%s", "1", s);

  /* Verify there is no warning with strings of length that cannot
     exceed 4k (because of the array size).  */
  T (-1, "%-s", ar->a1);
  T (-1, "%-s", ar->a4k);

  /* Verify there's no "exceeds minimum required size of 4095" warning
     with multiple %s directives and a combination of strings of unknown
     (and potentially unbounded) length and strings whose length is
     bounded by the size of the arrays they are stored in.  */
  T (-1, "%s%s", s, ar->a4k);
  T (-1, "%s%s", ar->a4k, s);
  T (-1, "%s%s", ar->a4k, ar->a4k);
  T (-1, "%s%s", ar->a4k, "123");
  T (-1, "%s%s", "123", ar->a4k);
  T (-1, "%s%s", ar->ax, ar->a4k);
  T (-1, "%s%s", ar->a4k, ar->ax);

  /* Verify that an array that fits a string longer than 4095 bytes
     does not trigger a warning.  (No known implementation has trouble
     with this).  */
  T (-1, "%s", ar->a4kp1);

  /* Verify that a %s directive with width greater than 4095 does
     trigger a warning even if the string argument is not longer
     than 4k.  Glibc only has trouble with directives whose width
     or precision exceeds 64K or so:
     https://bugzilla.redhat.com/show_bug.cgi?id=441945 *
     but hardcoding that as the limit and assuming no other
     implementation has a lower one seems unwise.  */
  T (-1, "%*s", 4096, ar->a4k);   /* { dg-warning "directive output of 4096 bytes exceeds minimum required size of 4095" } */

  /* Verify that precision constrains the putput and suppresses the 4k
     warning.  */
  T (-1, "%.*s", 4095, ar->a4kp1);

  T (-1, "%s %s", s, "");
  T (-1, "%s %s", "", s);
  T (-1, "%s %s", s, "1");
  T (-1, "%s %s", "1", s);

  T (-1, "%s%s%s", s, "1", s);
  T (-1, "%s%s%s", "1", s, "1");
  T (-1, "%s%s%s", s, s, s);
  T (-1, "%*s%*s%*s", 4093, s, 4094, s, 4095, s);
  T (-1, "%s %s %s", s, s, s);
  T (-1, "%s %s %s", ar->a4k, ar->a4k, ar->a4k);
  T (-1, "%s %s %s", ar->ax, ar->ax, ar->ax);

  /* Similar to the above, verify there's no warning for an array
     just because its size is INT_MAX bytes.  */
  T (-1, "%s", ar->amax);
}
