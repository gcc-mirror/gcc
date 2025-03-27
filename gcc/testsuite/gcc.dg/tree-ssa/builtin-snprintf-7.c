/* Test to verify that snprintf can determine the correct range
   of lengths of dynamically constructed string arguments.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

void* memcpy (void*, const void*, size_t);

char* strcpy (char * restrict, const char * restrict);
int snprintf (char * restrict, size_t, const char *restrict, ...);

void sink (void*, ...);

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name, counter)						\
  CAT (CAT (CAT (call_ ## name ##_on_line_, __LINE__), _), counter)

#define FAIL(name, counter) do {			\
    extern void FAILNAME (name, counter) (void);	\
    FAILNAME (name, counter)();				\
  } while (0)

/* Macro to emit a call to funcation named
   call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that no such call appears in output.  */
#define VERIFY_ELIM(expr)						\
  if (!(expr)) FAIL (in_true_branch_not_eliminated, __COUNTER__); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define VERIFY_KEEP(expr)			\
  if (expr)					\
    FAIL (made_in_true_branch, __COUNTER__);	\
  else						\
    FAIL (made_in_false_branch, __COUNTER__)

#define ARGS(...) __VA_ARGS__

/* Each test macro expands to a new function to get around bug 81776
   - missing sprintf optimization due to pointer escape analysis.  */
#define ELIM(expect, dst, init, fmt, ...)		\
  void CAT (test_func_on_line_, __LINE__)(void)		\
  {							\
    memcpy (dst, init, sizeof (init) - 1);		\
    const int res = snprintf (0, 0, fmt, __VA_ARGS__);	\
    VERIFY_ELIM (expect res);				\
  } typedef void dummy_typedef

#define KEEP(expect, dst, init, fmt, ...)		\
  void CAT (test_func_on_line_, __LINE__)(void)		\
  {							\
    memcpy (dst, init, sizeof (init) - 1);		\
    const int ret = snprintf (0, 0, fmt, __VA_ARGS__);	\
    VERIFY_KEEP (expect ret);				\
  } typedef void dummy_typedef


/* Verify that conditions involving snprintf calls with a string
   of some minimum but otherwise unbounded length stored in an array
   of unknown bound are not folded unless the format string itself
   restricts the maximum.  The string could be longer than INT_MAX
   making the snprintf call fail and return a negative value.  */

extern char gax[];

KEEP (1 <=, gax, "1",  "%s", gax);
KEEP (2 <=, gax, "12", "%s", gax);
KEEP (3 <=, gax, "123", "%s", gax);

ELIM (3 ==, gax, "123", "%.3s", gax);
ELIM (5 ==, gax, "123", "%.3s%.2s", gax, gax);


/* Disabled.  The global pointer passed to memcpy as the destination
   might point at itself, i.e., gptr == &gptr is a valid argument to
   memcpy.

extern char *gptr;

KEEP (1 <=, gptr, "1",  "%s", gptr);
KEEP (2 <=, gptr, "12", "%s", gptr);
KEEP (3 <=, gptr, "123", "%s", gptr);

ELIM (3 ==, gptr, "123", "%.3s", gptr);
ELIM (5 ==, gptr, "123", "%.3s%.2s", gptr, gptr);

*/

/* Verify that conditions involving snprintf calls with a string
   of some minimum but otherwise unbounded length stored in an array
   of a known bound are folded.  The longest string that can be
   stored in such arrays is bounded by the size of the array.  */

extern char ga4[4];

ELIM (0 <=, ga4, "\0",   "%s", ga4);
ELIM (3 >=, ga4, "\0",   "%s", ga4);

ELIM (1 <=, ga4, "1",  "%s", ga4);
ELIM (0 <=, ga4, "1",  "%s", ga4 + 1);
ELIM (0 <=, ga4, "1",  "%s", &ga4[1]);

ELIM (3 >=, ga4, "1",  "%s", ga4);
ELIM (2 >=, ga4, "1",  "%s", ga4 + 1);
ELIM (2 >=, ga4, "1",  "%s", &ga4[1]);

ELIM (2 <=, ga4, "12", "%s", ga4);
ELIM (3 >=, ga4, "12", "%s", ga4);

ELIM (3 <=, ga4, "123", "%s", ga4);
ELIM (3 ==, ga4, "123", "%.3s", ga4);
ELIM (5 ==, ga4, "123", "%.3s%.2s", ga4, ga4);

/* Verify conditionals involving dynamically created strings of known
   length stored in local arrays.  */

#undef ELIM
#define ELIM(expect, N1, N2, init1, init2, fmt, ...)	\
  void CAT (test_func_on_line_, __LINE__)(int i)	\
  {							\
    char a1[N1], a2[N2];				\
    memcpy (a1, init1, sizeof (init1) - 1);		\
    memcpy (a2, init2, sizeof (init2) - 1);		\
    const int res = snprintf (0, 0, fmt, __VA_ARGS__);	\
    VERIFY_ELIM (expect res);				\
  } typedef void dummy_typedef

ELIM (0 ==, 2, 2, "\0", "\0",   "%s",         i ? a1 : a2);
ELIM (2 ==, 2, 2, "\0", "\0",   "s=%s",       i ? a1 : a2);

ELIM (1 ==, 2, 2, "a\0", "b\0", "%s",         i ? a1 : a2);
ELIM (3 ==, 2, 2, "a\0", "b\0", "s=%s",       i ? a1 : a2);

ELIM (2 ==, 3, 5, "ab\0", "cd\0", "%s",       i ? a1 : a2);
ELIM (3 ==, 3, 5, "ab\0", "cd\0", "%3s",      i ? a1 : a2);
ELIM (3 ==, 5, 5, "abcd\0", "efgh\0", "%.3s", i ? a1 : a2);

ELIM (3 ==, 4, 1, "abc\0", "", "%s",          i ? a1 : "def");
ELIM (4 ==, 1, 5, "", "efgh\0", "%s",         i ? "abcd" : a2);

ELIM (4 ==, 5, 5, "abcd\0", "efgh\0", "%s",   i < 0 ? a1 : 0 < i ? a2 : "ijkl");

/* { dg-final { scan-tree-dump-times "_not_eliminated" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_" 6 "optimized" } } */
