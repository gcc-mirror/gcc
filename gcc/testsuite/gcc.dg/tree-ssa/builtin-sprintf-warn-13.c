/* { dg-do compile }
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

/* Verify warnings and ranges for certain overflow.  */
void test_min_overflow (int i)
{
  T (0, "%#hho", i);            /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#1hho", i);           /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#2hho", i);           /* { dg-warning "between 2 and 4 bytes" } */
  T (0, "%#3hho", i);           /* { dg-warning "between 3 and 4 bytes" } */
  T (0, "%#4hho", i);           /* { dg-warning "writing 4 bytes" } */
  T (0, "%#hho", R (-1,  0));   /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#1hho", R (-1,  0));  /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#2hho", R (-1,  0));  /* { dg-warning "between 2 and 4 bytes" } */
  T (0, "%#3hho", R (-1,  0));  /* { dg-warning "between 3 and 4 bytes" } */
  T (0, "%#4hho", R (-1,  0));  /* { dg-warning "writing 4 bytes" } */
  T (0, "%#hho", R (-1,  1));   /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#1hho", R (-1,  1));  /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#2hho", R (-1,  1));  /* { dg-warning "between 2 and 4 bytes" } */
  T (0, "%#3hho", R (-1,  1));  /* { dg-warning "between 3 and 4 bytes" } */
  T (0, "%#4hho", R (-1,  1));  /* { dg-warning "writing 4 bytes" } */
  T (0, "%#hho", R ( 0,  1));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%#1hho", R ( 0,  1));  /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%#2hho", R ( 0,  1));  /* { dg-warning "writing 2 bytes" } */
  T (0, "%#3hho", R ( 0,  1));  /* { dg-warning "writing 3 bytes" } */
  T (0, "%#4hho", R ( 0,  1));  /* { dg-warning "writing 4 bytes" } */
  T (0, "%#hho", R ( 1,  2));   /* { dg-warning "writing 2 bytes" } */
  T (0, "%#1hho", R ( 1,  2));  /* { dg-warning "writing 2 bytes" } */
  T (0, "%#2hho", R ( 1,  2));  /* { dg-warning "writing 2 bytes" } */
  T (0, "%#3hho", R ( 1,  2));  /* { dg-warning "writing 3 bytes" } */
  T (0, "%#4hho", R ( 1,  2));  /* { dg-warning "writing 4 bytes" } */

  T (0, "%#ho",  i);            /* { dg-warning "between 1 and 7 bytes" } */
  T (0, "%#.*ho",               /* { dg-warning "between 1 and 7 bytes" } */
     R (0, 2), i);
  T (0, "%#.*ho",               /* { dg-warning "between 1 and 7 bytes" } */
     R (1, 2), i);
  T (0, "%#.*ho",               /* { dg-warning "between 2 and 7 bytes" } */
     R (2, 3), i);
  T (0, "%#.*ho",               /* { dg-warning "between 3 and 7 bytes" } */
     R (3, 4), i);
  T (0, "%#.*ho",               /* { dg-warning "between 7 and 8 bytes" } */
     R (7, 8), i);

  T (0, "%#ho",  R (-1,  0));   /* { dg-warning "between 1 and 7 bytes" } */
  T (0, "%#ho",  R (-1,  1));   /* { dg-warning "between 1 and 7 bytes" } */
  T (0, "%#ho",  R ( 0,  1));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%#ho",  R ( 1,  2));   /* { dg-warning "writing 2 bytes" } */

  T (0, "%#o",   i);            /* { dg-warning "between 1 and 12 bytes" } */
  T (0, "%#o",   R (-1,  0));   /* { dg-warning "between 1 and 12 bytes" } */
  T (0, "%#o",   R (-1,  1));   /* { dg-warning "between 1 and 12 bytes" } */
  T (0, "%#o",   R ( 0,  1));   /* { dg-warning "between 1 and 2 bytes" } */
  T (0, "%#o",   R ( 1,  2));   /* { dg-warning "writing 2 bytes" } */

  T (0, "%#hhx", i);            /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#.*hhx",              /* { dg-warning "writing up to 4 bytes" } */
     R (0, 2), i);
  T (0, "%#.*hhx",              /* { dg-warning "between 1 and 4 bytes" } */
     R (1, 2), i);
  T (0, "%#.*hhx",              /* { dg-warning "between 2 and 5 bytes" } */
     R (2, 3), i);
  T (0, "%#.*hhx",              /* { dg-warning "between 3 and 6 bytes" } */
     R (3, 4), i);

  T (0, "%#hhx", R (-1,  0));   /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#hhx", R (-1,  1));   /* { dg-warning "between 1 and 4 bytes" } */
  T (0, "%#hhx", R ( 0,  1));   /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%#hhx", R ( 1,  2));   /* { dg-warning "writing 3 bytes" } */

  T (0, "%#hx", i);             /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%#hx", R (-1,  0));    /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%#hx", R (-1,  1));    /* { dg-warning "between 1 and 6 bytes" } */
  T (0, "%#hx", R ( 0,  1));    /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%#hx", R ( 1,  2));    /* { dg-warning "writing 3 bytes" } */

  T (0, "%#x",   i);            /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%#x",   R (-1,  0));   /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%#x",   R (-1,  1));   /* { dg-warning "between 1 and 10 bytes" } */
  T (0, "%#x",   R ( 0,  1));   /* { dg-warning "between 1 and 3 bytes" } */
  T (0, "%#x",   R ( 1,  2));   /* { dg-warning "writing 3 bytes" } */
}

/* Verify warnings and ranges for likely overflow.  */
void test_likely_overflow (int i)
{
  T (2, "%#hho", i);          /* { dg-warning "may write a terminating nul" } */
  T (2, "%#1hho", i);         /* { dg-warning "may write a terminating nul" } */
  T (2, "%#2hho", i);         /* { dg-warning "writing a terminating nul" } */
  T (2, "%#3hho", i);         /* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hho", i);         /* { dg-warning "writing 4 bytes" } */
  T (2, "%#hho", R (-1,  0)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#1hho", R (-1,  0));/* { dg-warning "may write a terminating nul" } */
  T (2, "%#2hho", R (-1,  0));/* { dg-warning "writing a terminating nul" } */
  T (2, "%#3hho", R (-1,  0));/* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hho", R (-1,  0));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hho", R (-1,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#1hho", R (-1,  1));/* { dg-warning "may write a terminating nul" } */
  T (2, "%#2hho", R (-1,  1));/* { dg-warning "writing a terminating nul" } */
  T (2, "%#3hho", R (-1,  1));/* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hho", R (-1,  1));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hho", R ( 0,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#1hho", R ( 0,  1));/* { dg-warning "may write a terminating nul" } */
  T (2, "%#2hho", R ( 0,  1));/* { dg-warning "writing a terminating nul" } */
  T (2, "%#3hho", R ( 0,  1));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#4hho", R ( 0,  1));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hho", R ( 1,  2)); /* { dg-warning "writing a terminating nul" } */
  T (2, "%#1hho", R ( 1,  2));/* { dg-warning "writing a terminating nul" } */
  T (2, "%#2hho", R ( 1,  2));/* { dg-warning "writing a terminating nul" } */
  T (2, "%#3hho", R ( 1,  2));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#4hho", R ( 1,  2));/* { dg-warning "writing 4 bytes" } */

  T (2, "%#ho",  i);          /* { dg-warning "may write a terminating nul" } */
  T (2, "%#ho",  R (-1,  0)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#ho",  R (-1,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#ho",  R ( 0,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#ho",  R ( 1,  2)); /* { dg-warning "writing a terminating nul" } */

  T (2, "%#o",   i);          /* { dg-warning "may write a terminating nul" } */
  T (2, "%#o",   R (-1,  0)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#o",   R (-1,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#o",   R ( 0,  1)); /* { dg-warning "may write a terminating nul" } */
  T (2, "%#o",   R ( 1,  2)); /* { dg-warning "writing a terminating nul" } */

  T (2, "%#hhx", i);          /* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#1hhx", i);         /* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#2hhx", i);         /* { dg-warning "between 2 and 4 bytes" } */
  T (2, "%#3hhx", i);         /* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hhx", i);         /* { dg-warning "writing 4 bytes" } */
  T (2, "%#1hhx", R (-1,  0));/* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#2hhx", R (-1,  0));/* { dg-warning "between 2 and 4 bytes" } */
  T (2, "%#3hhx", R (-1,  0));/* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hhx", R (-1,  0));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hhx", R (-1,  0)); /* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#1hhx", R (-1,  0));/* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#2hhx", R (-1,  0));/* { dg-warning "between 2 and 4 bytes" } */
  T (2, "%#3hhx", R (-1,  0));/* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hhx", R (-1,  0));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hhx", R (-1,  1)); /* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#1hhx", R (-1,  1));/* { dg-warning "between 1 and 4 bytes" } */
  T (2, "%#2hhx", R (-1,  1));/* { dg-warning "between 2 and 4 bytes" } */
  T (2, "%#3hhx", R (-1,  1));/* { dg-warning "between 3 and 4 bytes" } */
  T (2, "%#4hhx", R (-1,  1));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hhx", R ( 0,  1)); /* { dg-warning "between 1 and 3 bytes" } */
  T (2, "%#1hhx", R ( 0,  1));/* { dg-warning "between 1 and 3 bytes" } */
  T (2, "%#2hhx", R ( 0,  1));/* { dg-warning "between 2 and 3 bytes" } */
  T (2, "%#3hhx", R ( 0,  1));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#4hhx", R ( 0,  1));/* { dg-warning "writing 4 bytes" } */
  T (2, "%#hhx", R ( 1,  2)); /* { dg-warning "writing 3 bytes" } */
  T (2, "%#1hhx", R ( 1,  2));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#2hhx", R ( 1,  2));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#3hhx", R ( 1,  2));/* { dg-warning "writing 3 bytes" } */
  T (2, "%#4hhx", R ( 1,  2));/* { dg-warning "writing 4 bytes" } */

  T (2, "%#hx", i);           /* { dg-warning "between 1 and 6 bytes" } */
  T (2, "%#hx", R (-1,  0));  /* { dg-warning "between 1 and 6 bytes" } */
  T (2, "%#hx", R (-1,  1));  /* { dg-warning "between 1 and 6 bytes" } */
  T (2, "%#hx", R ( 0,  1));  /* { dg-warning "between 1 and 3 bytes" } */
  T (2, "%#hx", R ( 1,  2));  /* { dg-warning "writing 3 bytes" } */

  T (2, "%#x",   i);          /* { dg-warning "between 1 and 10 bytes" } */
  T (2, "%#x",   R (-1,  0)); /* { dg-warning "between 1 and 10 bytes" } */
  T (2, "%#x",   R (-1,  1)); /* { dg-warning "between 1 and 10 bytes" } */
  T (2, "%#x",   R ( 0,  1)); /* { dg-warning "between 1 and 3 bytes" } */
  T (2, "%#x",   R ( 1,  2)); /* { dg-warning "writing 3 bytes" } */
}

/* Verify warnings likely overflow due to the terminating nul.  */
void test_likely_nul_overflow (int i)
{
  T (3, "%#hho", i);
  T (3, "%#hho", R (-1,  0));
  T (3, "%#hho", R (-1,  1));
  T (3, "%#hho", R ( 0,  1));
  T (3, "%#hho", R ( 1,  2));

  T (3, "%#ho",  i);
  T (3, "%#ho",  R (-1,  0));
  T (3, "%#ho",  R (-1,  1));
  T (3, "%#ho",  R ( 0,  1));
  T (3, "%#ho",  R ( 1,  2));

  T (3, "%#o",   i);
  T (3, "%#o",   R (-1,  0));
  T (3, "%#o",   R (-1,  1));
  T (3, "%#o",   R ( 0,  1));
  T (3, "%#o",   R ( 1,  2));

  T (3, "%#hhx", i);          /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hhx", R (-1,  0)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hhx", R (-1,  1)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hhx", R ( 0,  1)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hhx", R ( 1,  2)); /* { dg-warning "writing a terminating nul" } */

  T (3, "%#hx", i);           /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hx", R (-1,  0));  /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hx", R (-1,  1));  /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hx", R ( 0,  1));  /* { dg-warning "may write a terminating nul" } */
  T (3, "%#hx", R ( 1,  2));  /* { dg-warning "writing a terminating nul" } */

  T (3, "%#x",   i);          /* { dg-warning "may write a terminating nul" } */
  T (3, "%#x",   R (-1,  0)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#x",   R (-1,  1)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#x",   R ( 0,  1)); /* { dg-warning "may write a terminating nul" } */
  T (3, "%#x",   R ( 1,  2)); /* { dg-warning "writing a terminating nul" } */
}
