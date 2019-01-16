/* Test to verify that the return value of calls to __builtin_sprintf
   is not folded if the call isn't fully specified, even if it would
   otherwise produce a known number of bytes on output, and that if
   the return value is in a known range the range is not made
   available to subsequent passes and doesn't affect branching and
   the removal of code.
   The test is compiled with warnings disabled to make sure the absence
   of optimizations does not depend on the presence of warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fprintf-return-value -fdump-tree-optimized -w" } */

#ifndef LINE
# define LINE 0
#endif

#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)

#define LONG_MAX   __LONG_MAX__
#define LONG_MIN   (-LONG_MAX - 1)

char *buf;
char buf8k [8192];

#define concat(a, b)   a ## b
#define CAT(a, b)      concat (a, b)

/* Calls to this function must not be eliminated.  */
void must_not_eliminate (void);

#define EQL(expect, size, fmt, ...)					\
  void __attribute__ ((noinline, noclone))				\
  CAT (test_on_line_, __LINE__)(void)					\
  {									\
    if (!LINE || LINE == __LINE__)					\
      {									\
	char *dst = size < 0 ? buf : buf8k + sizeof buf8k - size;	\
	int result = __builtin_sprintf (dst, fmt, __VA_ARGS__);		\
	if (result != expect)						\
	  must_not_eliminate ();					\
      }									\
  }

/* Verify that the return value or range or return values from the call
   to the formatted function is not treated as a constant or made available
   to subsequent optimization passes.  */
#define RNG(min, max, size, fmt, ...)					\
  void __attribute__ ((noinline, noclone))				\
  CAT (test_on_line_, __LINE__)(void)					\
  {									\
    if (!LINE || LINE == __LINE__)					\
      {									\
	char *dst = size < 0 ? buf : buf8k + sizeof buf8k - size;	\
	int result = __builtin_sprintf (dst, fmt, __VA_ARGS__);		\
	if (result < min || max < result)				\
	  must_not_eliminate ();					\
      }									\
  }

typedef __SIZE_TYPE__ size_t;

volatile int i;
volatile unsigned u;
volatile long li;
volatile unsigned long lu;
volatile size_t sz;
volatile char *str;

volatile double d;
volatile long double ld;

/* Verify that overflowing the destination object disables the return
   value optimization.  */
EQL (0, 0, "%c",  ' ');
EQL (0, 0, "%c",  i)
EQL (0, 0, "%-s", "");

EQL (1, 1, "%c",  'x');
EQL (1, 1, "%-s", "x");

/* Size of character constant must be larger than 2 for this to overflow.  */
#if __SIZEOF_INT__ > 2
EQL (1, 2, "%c",  'x');
#endif

EQL (4, 4, "%4c", 'x');

/* Verify that exceeding the environmental limit of 4095 bytes for
   a single conversion specification disables the return value
   folding.  */
EQL (   4096, sizeof buf8k, "%4096c", 'x');

EQL (INT_MAX, -1, "%*c", INT_MAX, 'x');

EQL (   4096, sizeof buf8k, "%4096.4094f", 1.0);
EQL (   4096, sizeof buf8k, "%.4094f",     1.0);
EQL (   4097, sizeof buf8k, "%.4095f",     1.0);

enum { imax2 = (INT_MAX / 2) * 2 };
EQL (imax2, -1, "%*c%*c", INT_MAX / 2, 'x', INT_MAX / 2, 'y');

/* Verify that range information for calls that overflow the destination
   isn't available.

     +-- lower bound of the tested range
     |   +-- upper bound of the tested range
     |   |   +-- size of destination buffer
     |   |   |  +-- format string
     |   |   |  |       +-- argument(s)
     |   |   |  |       |
     V   V   V  V       V  */
RNG (0,  0,  0, "%hhi", i)
RNG (0,  0,  1, "%hhi", i)
RNG (0,  1,  1, "%hhi", i)
RNG (0,  0,  2, "%hhi", i)
RNG (0,  1,  2, "%hhi", i)
RNG (0,  2,  2, "%hhi", i)
RNG (0,  0,  3, "%hhi", i)
RNG (0,  1,  3, "%hhi", i)
RNG (0,  2,  3, "%hhi", i)
RNG (0,  3,  3, "%hhi", i)
RNG (0,  0,  4, "%hhi", i)
RNG (0,  1,  4, "%hhi", i)
RNG (0,  2,  4, "%hhi", i)
RNG (0,  3,  4, "%hhi", i)
RNG (0,  4,  4, "%hhi", i)

RNG (0,  0,  0, "%hhu", i)
RNG (0,  0,  1, "%hhu", i)
RNG (0,  1,  1, "%hhu", i)
RNG (0,  0,  2, "%hhu", i)
RNG (0,  1,  2, "%hhu", i)
RNG (0,  2,  2, "%hhu", i)
RNG (0,  0,  3, "%hhu", i)
RNG (0,  1,  3, "%hhu", i)
RNG (0,  2,  3, "%hhu", i)
RNG (0,  3,  3, "%hhu", i)

RNG (0,  0,  0, "%i", i)

RNG (0,  0,  1, "%i", i)
RNG (0,  1,  1, "%i", i)

RNG (0,  0,  2, "%i", i)
RNG (0,  1,  2, "%i", i)
RNG (0,  2,  2, "%i", i)

RNG (0,  0,  3, "%i", i)
RNG (0,  1,  3, "%i", i)
RNG (0,  2,  3, "%i", i)
RNG (0,  3,  3, "%i", i)

RNG (0,  0,  4, "%i", i)
RNG (0,  1,  4, "%i", i)
RNG (0,  2,  4, "%i", i)
RNG (0,  3,  4, "%i", i)
RNG (0,  4,  4, "%i", i)

RNG (0,  0,  5, "%i", i)
RNG (0,  1,  5, "%i", i)
RNG (0,  2,  5, "%i", i)
RNG (0,  3,  5, "%i", i)
RNG (0,  4,  5, "%i", i)
RNG (0,  5,  5, "%i", i)

RNG (0,  0,  6, "%i", i)
RNG (0,  1,  6, "%i", i)
RNG (0,  2,  6, "%i", i)
RNG (0,  3,  6, "%i", i)
RNG (0,  4,  6, "%i", i)
RNG (0,  5,  6, "%i", i)
RNG (0,  6,  6, "%i", i)

/* If int is 16bit then it will always fit in 7 char characters with this
   formatting.  */
#if __SIZEOF_INT__ > 2
RNG (0,  0,  7, "%i", i)
#endif
RNG (0,  1,  7, "%i", i)
RNG (0,  2,  7, "%i", i)
RNG (0,  3,  7, "%i", i)
RNG (0,  4,  7, "%i", i)
RNG (0,  5,  7, "%i", i)
RNG (0,  6,  7, "%i", i)

RNG (4,  4, 32, "%i", i)
RNG (4,  4, 32, "%u", u)
RNG (4,  4, 32, "%li", li)
RNG (4,  4, 32, "%lu", lu)
RNG (4,  4, 32, "%zu", sz)

/* Exercise bug 78586.  */
RNG (4,  4, 32, "%lu", (unsigned long)i)
RNG (4,  4, 32, "%lu", (unsigned long)u)
RNG (4,  4, 32, "%lu", (unsigned long)li)
RNG (4,  4, 32, "%lu", (unsigned long)lu)
RNG (4,  4, 32, "%lu", (unsigned long)sz)


#if __SIZEOF_INT__ == 4

/* A 32-bit int takes up at most 11 bytes (-2147483648) not including
   the terminating nul.  */
RNG (0,  7,  7, "%i", i)

RNG (0,  0,  8, "%i", i)
RNG (0,  1,  8, "%i", i)
RNG (0,  2,  8, "%i", i)
RNG (0,  3,  8, "%i", i)
RNG (0,  4,  8, "%i", i)
RNG (0,  5,  8, "%i", i)
RNG (0,  6,  8, "%i", i)
RNG (0,  7,  8, "%i", i)
RNG (0,  8,  8, "%i", i)

RNG (0,  0,  9, "%i", i)
RNG (0,  1,  9, "%i", i)
RNG (0,  2,  9, "%i", i)
RNG (0,  3,  9, "%i", i)
RNG (0,  4,  9, "%i", i)
RNG (0,  5,  9, "%i", i)
RNG (0,  6,  9, "%i", i)
RNG (0,  7,  9, "%i", i)
RNG (0,  8,  9, "%i", i)
RNG (0,  9,  9, "%i", i)

RNG (0,  0, 10, "%i", i)
RNG (0,  1, 10, "%i", i)
RNG (0,  2, 10, "%i", i)
RNG (0,  3, 10, "%i", i)
RNG (0,  4, 10, "%i", i)
RNG (0,  5, 10, "%i", i)
RNG (0,  6, 10, "%i", i)
RNG (0,  7, 10, "%i", i)
RNG (0,  8, 10, "%i", i)
RNG (0,  9, 10, "%i", i)
RNG (0, 10, 10, "%i", i)

#endif

/* Verify that the output of a "%a" directive with no precision is not
   considered constant or within a known range (the number of digits
   after the decimal point is unspecified in this case).  The hardcoded
   ranges correspond to Glibc values.  */
RNG (6,  6,  7, "%a",       0.0)    /* Glibc output: "0x0p+0"  */
RNG (6,  6,  7, "%a",       d)
RNG (6,  6,  7, "%.4096a",  d)

RNG (6,  6,  7, "%La",      0.0L)   /* Glibc output: "0x0p+0"  */
RNG (6,  6,  7, "%La",      ld)
RNG (6,  6,  7, "%.4096La", ld)

/* Verify that the pound flag with unknown precision prevents the %g
   directive from trimming trailing zeros as it otherwise does.  As
   a consequence, the result must be assumed to be as large as
   precision.  */
RNG (1,  315,  316, "%#.*g", i, d);
RNG (1, 4095, 4096, "%#.*g", i, d);
RNG (1, 4095, 4096, "%#.*g", i, 0.0);

/* Verify that the result of formatting an unknown string isn't optimized
   into a non-negative range.  The string could be longer that 4,095 bytes,
   resulting in the formatting function having undefined behavior (and
   returning a negative value as Glibc can for some directives).  */
RNG (0,  INT_MAX, -1, "%-s", str);

/* Verify the result of a conditional expression involving a string
   literal and an unknown string isn't optimized.  */
RNG (0,  1,   4, "%-s", i ? str : "123");
RNG (0,  1,   4, "%-s", i ? "123" : str);

/* Verfy that the output involving wide strings is not optimized
   (the output is actually bounded by a function of MB_LEN_MAX
   which should be at least 6 to accommodate UTF-8 but this isn't
   implemented yet).  */
RNG (0,  5,   7, "%ls",   L"1");
RNG (0,  6,   8, "%s%ls", "1", L"2");

/* Verify that no call to abort has been eliminated and that each call
   is at the beginning of a basic block (and thus the result of a branch).
   This latter test tries to verify that the test preceding the call to
   the must_not_eliminate() function has not been eliminated either.

   The expected output looks something like this:

   <bb 2>:
   result_3 = __builtin_sprintf (&MEM[(void *)&buf8k + 8192B], "%c", 32);
   if (result_3 != 0)
     goto <bb 3>; [50.0%] [count: INV]
   else
     goto <bb 4>; [50.0%] [count: INV]

   <bb 3>[50.0%] [count: INV]:
   must_not_eliminate ();

*/

/*  Only conditional calls to must_not_eliminate must be made (with
    any probability):
    { dg-final { scan-tree-dump-times "> \\\[local count: \[0-9INV\]*\\\]:\n *must_not_eliminate" 127 "optimized" { target { ilp32 || lp64 } } } }
    { dg-final { scan-tree-dump-times "> \\\[local count: \[0-9INV\]*\\\]:\n *must_not_eliminate" 94 "optimized" { target { ! { ilp32 || lp64 } } } } }
    No unconditional calls to abort should be made:
    { dg-final { scan-tree-dump-not ";\n *must_not_eliminate" "optimized" } } */
