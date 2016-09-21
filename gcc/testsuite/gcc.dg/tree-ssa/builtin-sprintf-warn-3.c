/* { dg-do compile } */
/* { dg-options "-std=c99 -O2 -Wformat -Wformat-length=1 -ftrack-macro-expansion=0" } */

#ifndef LINE
#  define LINE 0
#endif

#define bos(x) __builtin_object_size (x, 0)

#define T(bufsize, fmt, ...)						\
    do {								\
      if (!LINE || __LINE__ == LINE)					\
	{								\
	  char *d = (char *)__builtin_malloc (bufsize);			\
	  __builtin___sprintf_chk (d, 0, bos (d), fmt, __VA_ARGS__);	\
	  sink (d);							\
	}								\
    } while (0)

void
sink (void*);

/* Identity function to verify that the checker figures out the value
   of the operand even when it's not constant (i.e., makes use of
   inlining and constant propagation information).  */

int i (int x) { return x; }
const char* s (const char *str) { return str; }

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

  T (1, "%s", x ? "" : "1");       /* { dg-warning "nul past the end" } */
  T (1, "%s", x ? "1" : "");       /* { dg-warning "nul past the end" } */
  T (1, "%s", x ? s : "1");        /* { dg-warning "nul past the end" } */
  T (1, "%s", x ? "1" : s);        /* { dg-warning "nul past the end" } */
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
  T ( 1, "%i_", i (    1));         /* { dg-warning "character ._. at offset 2 past the end" } */
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
  T ( 2, "_%i_",i (    0));         /* { dg-warning "character ._. at offset 3 past the end" } */
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

/* Functions to require optimization to figure out the range of the operand.
   Used to verify that the checker makes use of the range information to
   avoid diagnosing the output of sufficiently constrained arguments to
   integer directives.  */

signed char*
range_schar (signed char *val, signed char min, signed char max)
{
  if (*val < min || max < *val) __builtin_abort ();
  return val;
}

unsigned char*
range_uchar (unsigned char *val, unsigned char min, unsigned char max)
{
  if (*val < min || max < *val) __builtin_abort ();
  return val;
}

signed short*
range_sshort (signed short *val, signed short min, signed short max)
{
  if (*val < min || max < *val) __builtin_abort ();
  return val;
}

unsigned short*
range_ushort (unsigned short *val, unsigned short min, unsigned short max)
{
  if (*val < min || max < *val) __builtin_abort ();
  return val;
}

/* Helper to prevent GCC from figuring out the return value.  */
extern int idx (void);

/* Exercise ranges only in types signed and unsigned char and short.
   No other types work due to bug 71690.  */

void test_sprintf_chk_range_schar (signed char *a)
{
  (void)&a;

  /* Ra creates a range of signed char for A [idx].  A different
     value is used each time to prevent the ranges from intesecting
     one another, possibly even eliminating some tests as a result
     of the range being empty.  */
#define R(min, max) *range_schar (a + idx (), min, max)

  T ( 0, "%i",  R (0, 9));      /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  R (0, 9));      /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  R (0, 9));
  T ( 2, "%i",  R (-1, 0));     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 2, "%i",  R (9, 10));     /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  R ( -9,   9));
  T ( 3, "%i",  R (-99,  99));  /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 3, "%i",  R (  0,  99));
  T ( 3, "%i",  R (  0, 100));  /* { dg-warning "may write a terminating nul past the end of the destination" } */

  /* The following call may write as few as 3 bytes and as many as 5.
     It's judgment call how best to diagnose it to make the potential
     problem clear.  */
  T ( 3, "%i%i", R (1, 10), R (9, 10));   /* { dg-warning ".%i. directive writing between 1 and 2 bytes into a region of size 1" } */

  T ( 4, "%i%i", R (10, 11), R (12, 13));   /* { dg-warning "nul past the end" } */

  T ( 5, "%i%i", R (-9, 99), R (-9, 99));

  T ( 6, "%i_%i_%i", R (0, 9), R (0, 9), R (0,  9));
  T ( 6, "%i_%i_%i", R (0, 9), R (0, 9), R (0, 10));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%i_%i_%i", R (0, 9), R (0, 10), R (0, 9));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%i_%i_%i", R (0, 10), R (0, 9), R (0, 9));  /* { dg-warning "may write a terminating nul past the end" } */
  T ( 6, "%i_%i_%i", R (0, 9), R (0, 10), R (0, 10)); /* { dg-warning ".%i. directive writing between 1 and 2 bytes into a region of size 1" } */
}

void test_sprintf_chk_range_uchar (unsigned char *a, unsigned char *b)
{
  (void)&a;
  (void)&b;

#undef Ra
#define Ra(min, max) *range_uchar (a + idx (), min, max)

  T ( 0, "%i",  Ra (0,  9));   /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  Ra (0,  9));   /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  Ra (0,  9));
  T ( 2, "%i",  Ra (9, 10));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  Ra (0,  99));
  T ( 3, "%i",  Ra (0, 100));  /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_sprintf_chk_range_sshort (signed short *a, signed short *b)
{
  (void)&a;
  (void)&b;

#undef Ra
#define Ra(min, max) *range_sshort (a + idx (), min, max)

  T ( 0, "%i",  Ra ( 0, 9));     /* { dg-warning ".%i. directive writing 1 byte into a region of size 0" } */
  T ( 1, "%i",  Ra ( 0, 1));     /* { dg-warning "nul past the end" } */
  T ( 1, "%i",  Ra ( 0, 9));     /* { dg-warning "nul past the end" } */
  T ( 2, "%i",  Ra ( 0, 1));
  T ( 2, "%i",  Ra ( 8, 9));
  T ( 2, "%i",  Ra ( 0, 9));
  T ( 2, "%i",  Ra (-1, 0));     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T ( 2, "%i",  Ra ( 9, 10));    /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 3, "%i",  Ra ( 0, 99));
  T ( 3, "%i",  Ra (99, 999));   /* { dg-warning "may write a terminating nul past the end of the destination" } */

  T ( 4, "%i",  Ra (  0,  999));
  T ( 4, "%i",  Ra ( 99,  999));
  T ( 4, "%i",  Ra (998,  999));
  T ( 4, "%i",  Ra (999, 1000)); /* { dg-warning "may write a terminating nul past the end of the destination" } */
}
