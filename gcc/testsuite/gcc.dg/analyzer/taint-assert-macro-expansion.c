/* Integration test of how the execution path looks for
   -Wanalyzer-tainted-assertion with macro-tracking enabled
   (the default).  */

// TODO: remove need for this option
/* { dg-additional-options "-fanalyzer-checker=taint" } */

/* { dg-additional-options "-fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

/* An assertion macro that has a call to a __noreturn__ function.  */

extern void my_assert_fail (const char *expr, const char *file, int line)
  __attribute__ ((__noreturn__));

#define MY_ASSERT_1(EXPR) \
  do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0) /* { dg-warning "use of attacked-controlled value in condition for assertion \\\[CWE-617\\\] \\\[-Wanalyzer-tainted-assertion\\\]" } */

int __attribute__((tainted_args))
test_tainted_MY_ASSERT_1 (int n)
{
  MY_ASSERT_1 (n > 0);
  return n * n;
}

/* { dg-begin-multiline-output "" }
   do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
                     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
// note: in expansion of macro 'MY_ASSERT_1'
/* { dg-begin-multiline-output "" }
   MY_ASSERT_1 (n > 0);
   ^~~~~~~~~~~
  'test_tainted_MY_ASSERT_1': event 1 (depth 0)
    |
    | test_tainted_MY_ASSERT_1 (int n)
    | ^~~~~~~~~~~~~~~~~~~~~~~~
    | |
    | (1) function 'test_tainted_MY_ASSERT_1' marked with '__attribute__((tainted_args))'
    |
    +--> 'test_tainted_MY_ASSERT_1': event 2 (depth 1)
           |
           | test_tainted_MY_ASSERT_1 (int n)
           | ^~~~~~~~~~~~~~~~~~~~~~~~
           | |
           | (2) entry to 'test_tainted_MY_ASSERT_1'
           |
         'test_tainted_MY_ASSERT_1': event 3 (depth 1)
           |
           |   do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
           |           ^
           |           |
           |           (3) use of attacker-controlled value for control flow
   { dg-end-multiline-output "" } */
// note: in expansion of macro 'MY_ASSERT_1'
/* { dg-begin-multiline-output "" }
           |   MY_ASSERT_1 (n > 0);
           |   ^~~~~~~~~~~
           |
         'test_tainted_MY_ASSERT_1': event 4 (depth 1)
           |
           |   do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
           |           ^
           |           |
           |           (4) following 'true' branch (when 'n <= 0')...
   { dg-end-multiline-output "" } */
// note: in expansion of macro 'MY_ASSERT_1'
/* { dg-begin-multiline-output "" }
           |   MY_ASSERT_1 (n > 0);
           |   ^~~~~~~~~~~
           |
         'test_tainted_MY_ASSERT_1': event 5 (depth 1)
           |
           |   do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
           |                     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |                     |
           |                     (5) ...to here
   { dg-end-multiline-output "" } */
// note: in expansion of macro 'MY_ASSERT_1'
/* { dg-begin-multiline-output "" }
           |   MY_ASSERT_1 (n > 0);
           |   ^~~~~~~~~~~
           |
         'test_tainted_MY_ASSERT_1': event 6 (depth 1)
           |
           |   do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
           |                     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |                     |
           |                     (6) treating 'my_assert_fail' as an assertion failure handler due to '__attribute__((__noreturn__))'
   { dg-end-multiline-output "" } */
// note: in expansion of macro 'MY_ASSERT_1'
/* { dg-begin-multiline-output "" }
           |   MY_ASSERT_1 (n > 0);
           |   ^~~~~~~~~~~
           |
   { dg-end-multiline-output "" } */
