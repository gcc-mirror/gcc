/* Integration test of how the execution path looks for
   -Wanalyzer-tainted-assertion with macro-tracking enabled
   (the default), where the assertion macro is defined in
   a system header.  */

// TODO: remove need for this option
/* { dg-additional-options "-fanalyzer-checker=taint" } */

/* { dg-additional-options "-fdiagnostics-show-path-depths" } */
/* { dg-additional-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */

/* An assertion macro that has a call to a __noreturn__ function.  */

/* This is marked as a system header.  */
#include "test-assert.h"

int __attribute__((tainted_args))
test_tainted_assert (int n)
{
  assert (n > 0); /* { dg-warning "use of attacked-controlled value in condition for assertion \\\[CWE-617\\\] \\\[-Wanalyzer-tainted-assertion\\\]" } */
  return n * n;
}

/* { dg-begin-multiline-output "" }
   assert (n > 0);
   ^~~~~~
  'test_tainted_assert': event 1 (depth 0)
    |
    | test_tainted_assert (int n)
    | ^~~~~~~~~~~~~~~~~~~
    | |
    | (1) function 'test_tainted_assert' marked with '__attribute__((tainted_args))'
    |
    +--> 'test_tainted_assert': event 2 (depth 1)
           |
           | test_tainted_assert (int n)
           | ^~~~~~~~~~~~~~~~~~~
           | |
           | (2) entry to 'test_tainted_assert'
           |
         'test_tainted_assert': events 3-6 (depth 1)
           |
           |
           |   do { if (!(EXPR)) __assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
           |           ^         ~~~~~~~~~~~~~
           |           |         |
           |           |         (5) ...to here
           |           |         (6) treating '__assert_fail' as an assertion failure handler due to '__attribute__((__noreturn__))'
           |           (3) use of attacker-controlled value for control flow
           |           (4) following 'true' branch (when 'n <= 0')...
           |
   { dg-end-multiline-output "" } */
