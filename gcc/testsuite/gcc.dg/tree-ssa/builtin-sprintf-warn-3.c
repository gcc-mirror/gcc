/* Verify that all sprintf built-ins detect overflow involving directives
   with non-constant arguments known to be constrained by some range of
   values, and even when writing into dynamically allocated buffers.
   -O2 (-ftree-vrp) is necessary for the tests involving ranges to pass,
   otherwise -O1 is sufficient.
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Wformat -Wformat-overflow=1 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

#ifndef LINE
#  define LINE 0
#endif

#define bos(x) __builtin_object_size (x, 0)

/* Defined (and redefined) to the allocation function to use, either
   malloc, or alloca, or a VLA.  */
#define ALLOC(p, n)   (p) = __builtin_malloc (n)

/* Defined (and redefined) to the sprintf function to exercise.  */
#define TEST_SPRINTF(d, maxsize, objsize, fmt, ...)		\
  __builtin___sprintf_chk (d, 0, objsize, fmt, __VA_ARGS__)

#define T(bufsize, fmt, ...)				\
  do {							\
    if (!LINE || __LINE__ == LINE)			\
      {							\
	char *d;					\
	ALLOC (d, bufsize);				\
	TEST_SPRINTF (d, 0, bos (d), fmt, __VA_ARGS__);	\
	sink (d);					\
      }							\
  } while (0)

void sink (void*);

/* Identity function to verify that the checker figures out the value
   of the operand even when it's not constant (i.e., makes use of
   inlining and constant propagation information).  */

static int i (int x) { return x; }
static const char* s (const char *str) { return str; }

/* Function to "generate" a unique unknown number (as far as GCC can
   tell) each time it's called.  It prevents the optimizer from being
   able to narrow down the ranges of possible values in test functions
   with repeated references to the same variable.  */
extern int x (void);

/* Verify that the checker can detect buffer overflow when the "%s"
   argument is in a known range of lengths and one or both of which
   exceed the size of the destination.  */

void test_sprintf_chk_string (const char *s, const char *t)
{
#define x x ()

  T (1, "%-s", x ? "" : "1");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : "");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : "1");        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : s);        /* { dg-warning "nul past the end" } */

  /* When neither string is known no warning should be issued at level 1
     since their lenghts are assumed to be zero.  */
  T (1, "%s", x ? s : t);

  T (2, "%s", x ? "" : "1");
  T (2, "%s", x ? "" : s);
  T (2, "%s", x ? "1" : "");
  T (2, "%s", x ? s : "");
  T (2, "%s", x ? "1" : "2");
  T (2, "%s", x ? "" : "12");      /* { dg-warning "nul past the end" } */
  T (2, "%s", x ? "12" : "");      /* { dg-warning "nul past the end" } */

  T (2, "%s", x ? "" : "123");     /* { dg-warning "into a region" } */
  T (2, "%s", x ? "123" : "");     /* { dg-warning "into a region" } */

#undef x
}


/* Verify that the checker makes use of integer constant propagation
   to detect buffer overflow in non-constant cases.  */

void test_sprintf_chk_integer_value (void)
{
  T ( 1, "%i",  i (    0));         /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  i (    1));         /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  i (   -1));         /* { dg-warning "into a region" } */
  T ( 1, "%i_", i (    1));         /* { dg-warning " 1 byte into a region of size 0" } */
  T ( 1, "_%i", i (    1));         /* { dg-warning "into a region" } */
  T ( 1, "_%i_",i (    1));         /* { dg-warning "into a region" } */
  T ( 1, "%o",  i (    0));         /* { dg-warning "nul past the end" } */
  T ( 1, "%u",  i (    0));         /* { dg-warning "nul past the end" } */
  T ( 1, "%x",  i (    0));         /* { dg-warning "nul past the end" } */
  T ( 1, "%#x", i (    0));         /* { dg-warning "nul past the end" } */
  T ( 1, "%x",  i (    1));         /* { dg-warning "nul past the end" } */
  T ( 1, "%#x", i (    1));         /* { dg-warning "into a region" } */

  T ( 2, "%i",  i (    0));
  T ( 2, "%i",  i (    1));
  T ( 2, "%i",  i (    9));
  T ( 2, "%i",  i (   -1));         /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  i (   10));         /* { dg-warning "nul past the end" } */
  T ( 2, "%i_", i (    0));         /* { dg-warning "nul past the end" } */
  T ( 2, "_%i", i (    0));         /* { dg-warning "nul past the end" } */
  T ( 2, "_%i_",i (    0));         /* { dg-warning " 1 byte into a region of size 0" } */
  T ( 2, "%o",  i (    1));
  T ( 2, "%o",  i (    7));
  T ( 2, "%o",  i (  010));         /* { dg-warning "nul past the end" } */
  T ( 2, "%o",  i ( 0100));         /* { dg-warning "into a region" } */
  T ( 2, "%x",  i (    1));
  T ( 2, "%#x", i (    1));         /* { dg-warning "into a region" } */
  T ( 2, "%x",  i (  0xa));
  T ( 2, "%x",  i (  0xf));
  T ( 2, "%x",  i ( 0x10));         /* { dg-warning "nul past the end" } */
  T ( 2, "%x",  i ( 0xff));         /* { dg-warning "nul past the end" } */
  T ( 2, "%x",  i (0x1ff));         /* { dg-warning "into a region" } */

  T ( 3, "%i",  i (    0));
  T ( 3, "%i",  i (    1));
  T ( 3, "%i",  i (    9));
  T ( 3, "%i",  i (   -9));
  T ( 3, "%i",  i (   10));
  T ( 3, "%i",  i (   99));
  T ( 3, "%i",  i (  -99));         /* { dg-warning "nul past the end" } */

  T ( 3, "%i",  i (99) + i (1));    /* { dg-warning "nul past the end" } */

  T ( 8, "%8u", i (    1));         /* { dg-warning "nul past the end" } */
  T ( 9, "%8u", i (    1));
}

extern int rand (void);

/* Functions to require optimization to figure out the range of the operand.
   Used to verify that the checker makes use of the range information to
   avoid diagnosing the output of sufficiently constrained arguments to
   integer directives.  */

static signed char
range_schar (signed char min, signed char max)
{
  signed char val = rand ();
  return val < min || max < val ? min : val;
}

static unsigned char
range_uchar (unsigned char min, unsigned char max)
{
  unsigned char val = rand ();
  return val < min || max < val ? min : val;
}

static signed short
range_sshrt (signed short min, signed short max)
{
  signed short val = rand ();
  return val < min || max < val ? min : val;
}

static unsigned short
range_ushrt (unsigned short min, unsigned short max)
{
  unsigned short val = rand ();
  return val < min || max < val ? min : val;
}

static signed int
range_sint (signed int min, signed int max)
{
  signed int val = rand ();
  return val < min || max < val ? min : val;
}

static unsigned int
range_uint (unsigned int min, unsigned int max)
{
  unsigned int val = rand ();
  return val < min || max < val ? min : val;
}

void test_sprintf_chk_range_schar (void)
{
#define R(min, max) range_sint (min, max)

  T ( 0, "%hhi", R (0, 1));     /* { dg-warning ".%hhi. directive writing 1 byte into a region of size 0" } */
  /* { dg-message "directive argument in the range \\\[0, 1\\\]" "note" { target *-*-* } .-1 } */

  T ( 0, "%hhi", R (0, 127));   /* { dg-warning ".%hhi. directive writing between 1 and 3 bytes into a region of size 0" } */
  /* { dg-message "directive argument in the range \\\[0, 127\\\]" "note" { target *-*-* } .-1 } */

  T ( 0, "%hhi", R (1024, 1033));   /* { dg-warning ".%hhi. directive writing 1 byte into a region of size 0" } */
  /* { dg-message "directive argument in the range \\\[1024, 1033\\\]" "note" { target *-*-* } .-1 } */

  T ( 0, "%hhi", R (1024, 1034));   /* { dg-warning ".%hhi. directive writing between 1 and 2 bytes into a region of size 0" } */
  /* { dg-message "directive argument in the range \\\[1024, 1034\\\]" "note" { target *-*-* } .-1 } */

  T ( 0, "%hhi", R (1024, 2035));   /* { dg-warning ".%hhi. directive writing between 1 and 4 bytes into a region of size 0" } */
  /* { dg-message "using the range \\\[-128, 127\\\] for directive argument" "note" { target *-*-* } .-1 } */

  T ( 2, "%#hhx", R (1234, 12345));  /* { dg-warning "'%#hhx' directive writing between 1 and 4 bytes into a region of size 2 " } */
  T ( 3, "%#hhx", R (1234, 12345));  /* { dg-warning "may write a terminating nul" } */
  T ( 4, "%#hhx", R (1234, 12345));

#undef R
#define R(min, max) range_schar (min, max)

  T ( 0, "%i",  R (0, 9));      /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R (0, 9));      /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R (0, 9));
  T ( 2, "%i",  R (-1, 0));     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 2, "%i",  R (9, 10));     /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( -9,   9));
  T ( 3, "%i",  R (-99,  99));  /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 3, "%i",  R (  0,  99));
  T ( 3, "%i",  R (  0, 100));  /* { dg-warning "may write a terminating nul past the end of the destination" } */

  /* The following call may write as few as 2 bytes and as many as 4.
     It's a judgment call how best to diagnose it to make the potential
     problem clear.  */
  T ( 3, "%i%i", R (1, 10), R (9, 10));   /* { dg-warning "directive writing between 1 and 2 bytes into a region of size between 1 and 2" } */

  T ( 4, "%i%i", R (10, 11), R (12, 13));   /* { dg-warning "nul past the end" } */

  T ( 5, "%i%i", R (-9, 99), R (-9, 99));

  T ( 6, "%i_%i_%i", R (0, 9), R (0, 9), R (0,  9));
  T ( 6, "%i_%i_%i", R (0, 9), R (0, 9), R (0, 10));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%i_%i_%i", R (0, 9), R (0, 10), R (0, 9));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%i_%i_%i", R (0, 10), R (0, 9), R (0, 9));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%hhi_%hi_%i", R (0, 9), R (0, 10), R (0, 10)); /* { dg-warning ".i. directive writing between 1 and 2 bytes into a region of size between 1 and 2" } */
  T ( 6, "%3i|%2i/%1i", R (0, 99), R (0, 99), R (0, 99)); /* { dg-warning "./. directive writing 1 byte into a region of size 0" } */
  T ( 6, "%.3i|%.2i/%i", R (0, 99), R (0, 99), R (0, 99)); /* { dg-warning "./. directive writing 1 byte into a region of size 0" } */
  T ( 6, "%.3i|%.2i/%i", R (0, 119), R (0, 99), R (0, 99)); /* { dg-warning "./. directive writing 1 byte into a region of size 0" } */
  T ( 6, "%.3i|%.2i/%i", R (0, 1), R (0, 2), R (0, 3)); /* { dg-warning "./. directive writing 1 byte into a region of size 0" } */
}

void test_sprintf_chk_range_uchar (void)
{
#undef R
#define R(min, max) range_uchar (min, max)

  T ( 0, "%i",  R (0,  9));   /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R (0,  9));   /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R (0,  9));
  T ( 2, "%i",  R (9, 10));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R (0,  99));
  T ( 3, "%i",  R (0, 100));  /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_sprintf_chk_range_sshrt (void)
{
#undef R
#define R(min, max) range_sshrt (min, max)

  T ( 0, "%i",  R ( 0, 9));     /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R ( 0, 1));     /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  R ( 0, 9));     /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R ( 0, 1));
  T ( 2, "%i",  R ( 8, 9));
  T ( 2, "%i",  R ( 0, 9));
  T ( 2, "%i",  R (-1, 0));     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 2, "%i",  R ( 9, 10));    /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( 0, 99));
  T ( 3, "%i",  R (99, 999));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 4, "%i",  R (  0,  999));
  T ( 4, "%i",  R ( 99,  999));
  T ( 4, "%i",  R (998,  999));
  T ( 4, "%i",  R (999, 1000)); /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_sprintf_chk_range_ushrt (void)
{
#undef R
#define R(min, max) range_ushrt (min, max)

  T ( 0, "%i",  R ( 0, 9));     /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R ( 0, 1));     /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  R ( 0, 9));     /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R ( 0, 1));
  T ( 2, "%i",  R ( 8, 9));
  T ( 2, "%i",  R ( 0, 9));
  T ( 2, "%i",  R ( 9, 10));    /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( 0, 99));
  T ( 3, "%i",  R (99, 999));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 4, "%i",  R (  0,  999));
  T ( 4, "%i",  R ( 99,  999));
  T ( 4, "%i",  R (998,  999));
  T ( 4, "%i",  R (999, 1000)); /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_sprintf_chk_range_sint (void)
{
#undef R
#define R(min, max) range_sint (min, max)

  T ( 0, "%i",  R ( 0, 9));     /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R ( 0, 1));     /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  R ( 0, 9));     /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R ( 0, 1));
  T ( 2, "%i",  R ( 8, 9));
  T ( 2, "%i",  R ( 0, 9));
  T ( 2, "%i",  R (-1, 0));     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 2, "%i",  R ( 9, 10));    /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( 0, 99));
  T ( 3, "%i",  R (99, 999));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 4, "%i",  R (  0,  999));
  T ( 4, "%i",  R ( 99,  999));
  T ( 4, "%i",  R (998,  999));
  T ( 4, "%i",  R (999, 1000)); /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_sprintf_chk_range_uint (void)
{
#undef R
#define R(min, max) range_uint (min, max)

  T ( 0, "%i",  R ( 0, 9));     /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R ( 0, 1));     /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  R ( 0, 9));     /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R ( 0, 1));
  T ( 2, "%i",  R ( 8, 9));
  T ( 2, "%i",  R ( 0, 9));
  T ( 2, "%i",  R ( 9, 10));    /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( 0, 99));
  T ( 3, "%i",  R (99, 999));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 4, "%i",  R (  0,  999));
  T ( 4, "%i",  R ( 99,  999));
  T ( 4, "%i",  R (998,  999));
  T ( 4, "%i",  R (999, 1000)); /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

/* Verify that destination size in excess of INT_MAX (and, separately,
   in excess of the largest object) is diagnosed.  The former because
   the functions are defined only for output of at most INT_MAX and
   specifying a large upper bound defeats the bounds checking (and,
   on some implementations such as Solaris, causes the function to
   fail.  The latter because due to the limit of ptrdiff_t no object
   can be larger than PTRDIFF_MAX bytes.  */

void test_too_large (char *d, int x, __builtin_va_list va)
{
  const size_t imax = __INT_MAX__;
  const size_t imax_p1 = imax + 1;

  __builtin_snprintf (d, imax,    "%c", x);
  __builtin_snprintf (d, imax_p1, "%c", x);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "INT_MAX + 1" { target lp64 } } */
  /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" "INT_MAX + 1" { target { { avr-*-* } || ilp32 } } .-1 } */

  __builtin_vsnprintf (d, imax,    "%c", va);
  __builtin_vsnprintf (d, imax_p1, "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "INT_MAX + 1" { target lp64 } } */
  /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" "INT_MAX + 1" { target { { avr-*-* } || ilp32 } } .-1 } */

  __builtin___snprintf_chk (d, imax,    0, imax,    "%c", x);
  __builtin___snprintf_chk (d, imax_p1, 0, imax_p1, "%c", x);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "INT_MAX + 1" { target lp64 } } */
  /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" "INT_MAX + 1" { target { { avr-*-* } || ilp32 } } .-1 } */

  __builtin___vsnprintf_chk (d, imax,    0, imax,    "%c", va);
  __builtin___vsnprintf_chk (d, imax_p1, 0, imax_p1, "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "INT_MAX + 1" { target lp64 } } */
  /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size" "INT_MAX + 1" { target { { avr-*-* } || ilp32 } } .-1 } */

  const size_t ptrmax = __PTRDIFF_MAX__;
  const size_t ptrmax_m1 = ptrmax - 1;

  __builtin_snprintf (d, ptrmax_m1, "%c", x);  /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX - 1" { target lp64 } } */
  __builtin_snprintf (d, ptrmax, "  %c", x);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX" { target lp64 } } */

  __builtin_vsnprintf (d, ptrmax_m1, "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX - 1" { target lp64 } } */
  __builtin_vsnprintf (d, ptrmax,    "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX" { target lp64 } } */

  __builtin___snprintf_chk (d, ptrmax_m1, 0, ptrmax_m1, "%c", x);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX - 1" { target lp64 } } */
  __builtin___snprintf_chk (d, ptrmax,    0, ptrmax,    "%c", x);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX" { target lp64 } } */

  __builtin___vsnprintf_chk (d, ptrmax_m1, 0, ptrmax_m1, "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX - 1" { target lp64 } } */
  __builtin___vsnprintf_chk (d, ptrmax,    0, ptrmax,    "%c", va);   /* { dg-warning "specified bound \[0-9\]+ exceeds .INT_MAX." "PTRDIFF_MAX" { target lp64 } } */
}

/* Exercise ordinary sprintf with malloc.  */
#undef TEST_SPRINTF
#define TEST_SPRINTF(d, maxsize, objsize, fmt, ...)	\
  __builtin_sprintf (d, fmt, __VA_ARGS__)

void test_sprintf_malloc (const char *s, const char *t)
{
#define x x ()

  T (1, "%-s", x ? "" : "1");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : "");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : "1");        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : s);        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : t);

  T (2, "%-s", x ? "" : "1");
  T (2, "%-s", x ? "" : s);
  T (2, "%-s", x ? "1" : "");
  T (2, "%-s", x ? s : "");
  T (2, "%-s", x ? "1" : "2");
  T (2, "%-s", x ? "" : "12");      /* { dg-warning "nul past the end" } */
  T (2, "%-s", x ? "12" : "");      /* { dg-warning "nul past the end" } */

  T (2, "%-s", x ? "" : "123");     /* { dg-warning "into a region" } */
  T (2, "%-s", x ? "123" : "");     /* { dg-warning "into a region" } */

#undef x
}

/* Exercise ordinary sprintf with alloca.  */
#undef ALLOC
#define ALLOC(p, n) (p) = __builtin_alloca (n)

void test_sprintf_alloca (const char *s, const char *t)
{
#define x x ()

  T (1, "%-s", x ? "" : "1");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : "");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : "1");        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : s);        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : t);

  T (2, "%-s", x ? "" : "1");
  T (2, "%-s", x ? "" : s);
  T (2, "%-s", x ? "1" : "");
  T (2, "%-s", x ? s : "");
  T (2, "%-s", x ? "1" : "2");
  T (2, "%-s", x ? "" : "12");      /* { dg-warning "nul past the end" } */
  T (2, "%-s", x ? "12" : "");      /* { dg-warning "nul past the end" } */

  T (2, "%-s", x ? "" : "123");     /* { dg-warning "into a region" } */
  T (2, "%-s", x ? "123" : "");     /* { dg-warning "into a region" } */

#undef x
}

/* Exercise ordinary sprintf with a VLA.  */
#undef ALLOC
#define ALLOC(p, n) char vla [i (n)]; (p) = vla

void test_sprintf_vla (const char *s, const char *t)
{
#define x x ()

  T (1, "%-s", x ? "" : "1");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : "");       /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : "1");        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? "1" : s);        /* { dg-warning "nul past the end" } */
  T (1, "%-s", x ? s : t);

  T (2, "%-s", x ? "" : "1");
  T (2, "%-s", x ? "" : s);
  T (2, "%-s", x ? "1" : "");
  T (2, "%-s", x ? s : "");
  T (2, "%-s", x ? "1" : "2");
  T (2, "%-s", x ? "" : "12");      /* { dg-warning "nul past the end" } */
  T (2, "%-s", x ? "12" : "");      /* { dg-warning "nul past the end" } */

  T (2, "%-s", x ? "" : "123");     /* { dg-warning "into a region" } */
  T (2, "%-s", x ? "123" : "");     /* { dg-warning "into a region" } */

#undef x
}
