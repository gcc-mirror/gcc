/* { dg-do compile } */
/* { dg-options "-O2 -Wformat -Wformat-overflow=2 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)

#ifndef LINE
#  define LINE 0
#endif

int dummy_sprintf (char*, const char*, ...);
void sink (void*);

char buffer[4096];
char *ptr;

/* Helper to expand function to either __builtin_f or dummy_f to
   make debugging GCC easy.  */
#define FUNC(f)							\
  ((!LINE || LINE == __LINE__) ? __builtin_ ## f : dummy_ ## f)

/* Evaluate to an array of SIZE characters when non-negative, or to
   a pointer to an unknown object otherwise.  */
#define buffer(size)					\
  ((0 <= size) ? buffer + sizeof buffer - (size) : ptr)

#define T(bufsize, fmt, ...)						\
  do {									\
    char *buf = buffer (bufsize);					\
    FUNC (sprintf)(buf, fmt, __VA_ARGS__);				\
    sink (buf);								\
  } while (0)


/* Identity function to verify that the checker figures out the value
   of the operand even when it's not constant (i.e., makes use of
   inlining and constant propagation information).  */

int i (int x) { return x; }
const char* s (const char *str) { return str; }

/* Function to "generate" a unique unknown number (as far as GCC can
   tell) each time it's called.  It prevents the optimizer from being
   able to narrow down the ranges of possible values in test functions
   with repeated references to the same variable.  */
extern int value (void);

/* Return a value in the range [MIN, MAX].  */
int range (int min, int max)
{
  int val = value ();
  return val < min || max < val ? min : val;
}

#define R(min, max) range (min, max)

/* Verify that the checker can detect buffer overflow when the "%s"
   argument is in a known range of lengths and one or both of which
   exceed the size of the destination.  */

void test_sprintf_chk_string (const char *s)
{
  T (1, "%*s", R (0, 1), "");     /* { dg-warning "may write a terminating nul" } */
  T (1, "%*s", R (-2, -1), "");   /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*s", R (-3,  2), "");   /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*s", R (-4,  5), "");   /* { dg-warning "writing up to 5 bytes" } */

  T (1, "%*s", R ( -5, 6), "1");  /* { dg-warning "writing between 1 and 6 bytes" } */
  T (1, "%*s", R ( -6, 7), "12"); /* { dg-warning "writing between 2 and 7 bytes" } */

  T (1, "%.*s", R (0, 1), "");
  T (1, "%.*s", R (0, 1), s);     /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*s", R (-2, -1), "");
  T (1, "%.*s", R (-2, -1), s);   /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*s", R (-3,  2), "");
  T (1, "%.*s", R (-4,  5), "");

  T (1, "%.*s", R ( -5, 6), "1");  /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*s", R ( -6, 7), "12"); /* { dg-warning "writing up to 2 bytes " } */
  T (1, "%.*s", R (  1, 7), "12"); /* { dg-warning "writing between 1 and 2 bytes " } */
  T (1, "%.*s", R (  2, 7), "12"); /* { dg-warning "writing 2 bytes " } */

  T (1, "%*.*s", R (0, 1), R (0, 1), "");     /* { dg-warning "may write a terminating nul" } */
  T (1, "%*.*s", R (0, 2), R (0, 1), "");     /* { dg-warning "directive writing up to 2 bytes into a region of size 1" } */
  T (1, "%*.*s", R (0, 3), R (0, 1), "");     /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (0, 1), R (0, 1), "1");    /* { dg-warning "may write a terminating nul" } */
  T (1, "%*.*s", R (0, 2), R (0, 1), "1");    /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 1), "1");    /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (0, 1), R (0, 1), "12");   /* { dg-warning "may write a terminating nul" } */
  T (1, "%*.*s", R (0, 2), R (0, 1), "12");   /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 1), "12");   /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (0, 1), R (0, 1), "123");  /* { dg-warning "may write a terminating nul" } */
  T (1, "%*.*s", R (0, 2), R (0, 1), "123");  /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 1), "123");  /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 1), s);      /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (0, 1), R (0, 2), "123");  /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*.*s", R (0, 2), R (0, 2), "123");  /* { dg-warning "writing up to 2 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 2), "123");  /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 2), s);      /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (0, 1), R (0, 3), "123");  /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*.*s", R (0, 2), R (0, 3), "123");  /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 3), "123");  /* { dg-warning "writing up to 3 bytes" } */
  T (1, "%*.*s", R (0, 3), R (0, 3), s);      /* { dg-warning "writing up to 3 bytes" } */

  T (1, "%*.*s", R (1, 1), R (0, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 2), R (0, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 3), R (0, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 3), R (0, 3), s);      /* { dg-warning "writing between 1 and 3 bytes" } */

  T (1, "%*.*s", R (1, 1), R (1, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 2), R (1, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 3), R (1, 3), "123");  /* { dg-warning "writing between 1 and 3 bytes" } */
  T (1, "%*.*s", R (1, 3), R (1, 3), s);      /* { dg-warning "writing between 1 and 3 bytes" } */

  T (1, "%*.*s", R (2, 3), R (1, 3), "123");  /* { dg-warning "writing between 2 and 3 bytes" } */
  T (1, "%*.*s", R (3, 4), R (1, 3), "123");  /* { dg-warning "writing between 3 and 4 bytes" } */
  T (1, "%*.*s", R (4, 5), R (1, 3), "123");  /* { dg-warning "writing between 4 and 5 bytes" } */
  T (1, "%*.*s", R (2, 3), R (1, 3), s);      /* { dg-warning "writing between 2 and 3 bytes" } */
}

void test_sprintf_chk_int (int w, int p, int i)
{
  T (1, "%*d", w, 0);             /* { dg-warning "may write a terminating nul|directive writing between 1 and \[0-9\]+ bytes" } */
  T (1, "%*d", w, i);             /* { dg-warning "may write a terminating nul|directive writing between 1 and \[0-9\]+ bytes" } */

  T (1, "%*d", R (-1, 1), 0);     /* { dg-warning "writing a terminating nul" } */
  T (1, "%*d", R ( 0, 1), 0);     /* { dg-warning "writing a terminating nul" } */
  T (1, "%+*d", R ( 0, 1), 0);    /* { dg-warning "directive writing 2 bytes" } */
  T (1, "%+*u", R ( 0, 1), 0);    /* { dg-warning "writing a terminating nul" } */
  T (2, "%*d", R (-3, -2), 0);     /* { dg-warning "directive writing between 1 and 3 bytes" } */
  T (2, "%*d", R (-3, -1), 0);     /* { dg-warning "directive writing between 1 and 3 bytes" } */
  T (2, "%*d", R (-3,  0), 0);     /* { dg-warning "directive writing between 1 and 3 bytes" } */
  T (2, "%*d", R (-2, -1), 0);     /* { dg-warning "may write a terminating nul" } */
  T (2, "%*d", R (-2,  2), 0);     /* { dg-warning "may write a terminating nul" } */
  T (2, "%*d", R (-1,  2), 0);     /* { dg-warning "may write a terminating nul" } */
  T (2, "%*d", R ( 0,  2), 0);     /* { dg-warning "may write a terminating nul" } */
  T (2, "%*d", R ( 1,  2), 0);     /* { dg-warning "may write a terminating nul" } */

  T (1, "%.*d", p, 0);             /* { dg-warning "may write a terminating nul|directive writing up to \[0-9\]+ bytes" } */
  T (1, "%.*d", p, i);             /* { dg-warning "may write a terminating nul||directive writing up to \[0-9\]+ bytes" } */
  T (1, "%.*d", R (INT_MIN, -1), 0);     /* { dg-warning "writing a terminating nul" } */
  T (1, "%.*d", R (INT_MIN,  0), 0);     /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*d", R (-2, -1), 0);     /* { dg-warning "writing a terminating nul" } */
  T (1, "%.*d", R (-1,  1), 0);     /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*d", R ( 0,  1), 0);     /* { dg-warning "may write a terminating nul" } */
  T (1, "%.*d", R ( 0,  2), 0);     /* { dg-warning "directive writing up to 2 bytes" } */
  T (1, "%.*d", R ( 0,  INT_MAX - 1), 0);     /* { dg-warning "directive writing up to \[0-9\]+ bytes" } */
  T (1, "%.*d", R ( 1,  INT_MAX - 1), 0);     /* { dg-warning "directive writing between 1 and \[0-9\]+ bytes" } */
}

/* { dg-prune-output "flag used with .%.. gnu_printf format" } */
