/* Test to verify that the return value of calls to __builtin_sprintf
   is not folded if the call has undefined behavior even if it would
   otherwise produce a known number of bytes on output, and that if
   the return value is in a known range the range is not made
   available to subsequent passes and doesn't affect branching and
   the removal of code.
   The test is compiled with warnings disabled to make sure the absence
   of optimizations does not depend on the presence of warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fprintf-return-value -fdump-tree-optimized -ftrack-macro-expansion=0 -w" } */

#ifndef LINE
# define LINE 0
#endif

#define INT_MAX __INT_MAX__

char *buf;
char buf8k [8192];

#define concat(a, b)   a ## b
#define CAT(a, b)      concat (a, b)

#define EQL(expect, size, fmt, ...)					\
  void CAT (test_on_line_, __LINE__)(void)				\
  {									\
    if (!LINE || LINE == __LINE__)					\
      {									\
	char *dst = size < 0 ? buf : buf8k + sizeof buf8k - size;	\
	int result = __builtin_sprintf (dst, fmt, __VA_ARGS__);		\
	if (result != expect)						\
	  __builtin_abort ();						\
      }									\
  }

/* Verify that the return value or range or return values from the call
   to the formatted function is not treated as a constant or made available
   to subsequent optimization passes.  */
#define RNG(min, max, size, fmt, ...)					\
  void CAT (test_on_line_, __LINE__)(void)				\
  {									\
    if (!LINE || LINE == __LINE__)					\
      {									\
	char *dst = size < 0 ? buf : buf8k + sizeof buf8k - size;	\
	int result = __builtin_sprintf (dst, fmt, __VA_ARGS__);		\
	if (result < min || max < result)				\
	  __builtin_abort ();						\
      }									\
  }

extern int i;
extern long li;
extern char *str;

/* Verify that overflowing the destination object disables the return
   value optimization.  */
EQL (0, 0, "%c",  ' ');
EQL (0, 0, "%c",  i)
EQL (0, 0, "%-s", "");

EQL (1, 1, "%c",  'x');
EQL (1, 1, "%-s", "x");

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

/* Verify that range inforation for calls that overflow the destination
   isn't available.  */
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

RNG (0,  0,  7, "%i", i)
RNG (0,  1,  7, "%i", i)
RNG (0,  2,  7, "%i", i)
RNG (0,  3,  7, "%i", i)
RNG (0,  4,  7, "%i", i)
RNG (0,  5,  7, "%i", i)
RNG (0,  6,  7, "%i", i)

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

/* Verify the result of a conditional expression involving a string
   literal and an unknown string isn't optimized.  */
RNG (0,  1,   4, "%-s", i ? str : "123");
RNG (0,  1,   4, "%-s", i ? "123" : str);

/* Verify that no call to abort has been eliminated and that each call
   is at the beginning of a basic block (and thus the result of a branch).
   This latter test tries to verify that the test preceding the call to
   abort has not been eliminated either.

   The expected output looks something like this:

   <bb 2>:
   result_3 = __builtin_sprintf (&MEM[(void *)&buf8k + 8192B], "%c", 32);
   if (result_3 != 0)
     goto <bb 3>;
   else
     goto <bb 4>;

   <bb 3>:
   __builtin_abort ();

*/

/* { dg-final { scan-tree-dump-times ">:\n *__builtin_abort" 105 "optimized" { target { ilp32 || lp64 } } } } */
/* { dg-final { scan-tree-dump-times ">:\n *__builtin_abort" 74 "optimized" { target { { ! ilp32 } && { ! lp64 } } } } } */
