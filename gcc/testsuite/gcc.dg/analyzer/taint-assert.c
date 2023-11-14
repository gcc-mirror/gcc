/* We need this, otherwise the warnings are emitted inside the macros, which
   makes it hard to write the DejaGnu directives.  */
/* { dg-additional-options " -ftrack-macro-expansion=0" } */

#include "analyzer-decls.h"

/* An assertion macro that has a call to a __noreturn__ function.  */

extern void my_assert_fail (const char *expr, const char *file, int line)
  __attribute__ ((__noreturn__));

#define MY_ASSERT_1(EXPR) \
  do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)

int
test_not_tainted_MY_ASSERT_1 (int n)
{
  MY_ASSERT_1 (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}

int __attribute__((tainted_args))
test_tainted_MY_ASSERT_1 (int n)
{
  MY_ASSERT_1 (n > 0); /* { dg-warning "use of attacked-controlled value in condition for assertion \\\[CWE-617\\\] \\\[-Wanalyzer-tainted-assertion\\\]" "warning" } */
  /* { dg-message "treating 'my_assert_fail' as an assertion failure handler due to '__attribute__\\(\\(__noreturn__\\)\\)'" "final event" { target *-*-* } .-1 } */
  return n * n;
}


/* An assertion macro that has a call to __builtin_unreachable.  */

#define MY_ASSERT_2(EXPR)					\
  do { if (!(EXPR)) __builtin_unreachable (); } while (0)

int
test_not_tainted_MY_ASSERT_2 (int n)
{
  MY_ASSERT_2 (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}

int __attribute__((tainted_args))
test_tainted_MY_ASSERT_2 (int n)
{
  MY_ASSERT_2 (n > 0); /* { dg-warning "-Wanalyzer-tainted-assertion" "warning" } */
  /* { dg-message "treating '__builtin_unreachable' as an assertion failure handler" "final event" { target *-*-* } .-1 } */
  return n * n;
}


/* An assertion macro that's preprocessed away.
   The analyzer doesn't see this, so can't warn.  */

#define MY_ASSERT_3(EXPR)  do { } while (0)

int
test_not_tainted_MY_ASSERT_3 (int n)
{
  MY_ASSERT_3 (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}

int __attribute__((tainted_args))
test_tainted_MY_ASSERT_3 (int n)
{
  MY_ASSERT_3 (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}


/* A macro that isn't an assertion.  */

extern void do_something_benign ();

#define NOT_AN_ASSERT(EXPR) \
  do { if (!(EXPR)) do_something_benign (); } while (0)

int
test_not_tainted_NOT_AN_ASSERT (int n)
{
  NOT_AN_ASSERT (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}

int __attribute__((tainted_args))
test_tainted_NOT_AN_ASSERT (int n)
{
  NOT_AN_ASSERT (n > 0); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
  return n * n;
}


/* A condition that isn't an assertion.  */

int __attribute__((tainted_args))
test_tainted_condition (int n)
{
  if (n > 0) /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
    return 1;
  else
    return -1;
}


/* More complicated expressions in assertions.  */

int g;

void __attribute__((tainted_args))
test_compound_condition_in_assert_1 (int n)
{
  MY_ASSERT_1 ((n * 2) < (g + 3)); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_compound_condition_in_assert_2 (int x, int y)
{
  MY_ASSERT_1 (x < 100 && y < 100); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_compound_condition_in_assert_3 (int x, int y)
{
  MY_ASSERT_1 (x < 100 || y < 100); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_sanitized_expression_in_assert (int n)
{
  __analyzer_dump_state ("taint", n); /* { dg-warning "state: 'tainted'" } */
  if (n < 0 || n >= 100)
    return;
  __analyzer_dump_state ("taint", n); /* { dg-warning "state: 'stop'" } */  
  MY_ASSERT_1 (n < 200); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_sanitization_then_ok_assertion (unsigned n)
{
  if (n >= 100)
    return;

  /* Shouldn't warn here, as g isn't attacker-controlled.  */
  MY_ASSERT_1 (g > 42); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_good_assert_then_bad_assert (unsigned n)
{
  /* Shouldn't warn here, as g isn't attacker-controlled.  */
  MY_ASSERT_1 (g > 42); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */

  /* ...but n is: */
  MY_ASSERT_1 (n < 100); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_bad_assert_then_good_assert (unsigned n)
{
  MY_ASSERT_1 (n < 100); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
  MY_ASSERT_1 (g > 42); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
}


/* */

void __attribute__((tainted_args))
test_zero_MY_ASSERT_1 (unsigned n)
{
  if (n >= 100)
    MY_ASSERT_1 (0); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_nonzero_MY_ASSERT_1 (unsigned n)
{
  if (n >= 100)
    MY_ASSERT_1 (1); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_zero_MY_ASSERT_2 (unsigned n)
{
  if (n >= 100)
    MY_ASSERT_2 (0); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

void __attribute__((tainted_args))
test_nonzero_MY_ASSERT_2 (unsigned n)
{
  if (n >= 100)
    MY_ASSERT_2 (1); /* { dg-bogus "-Wanalyzer-tainted-assertion" } */
}


/* Assertions that call a subroutine to do validity checking.  */
 
static int
__analyzer_valid_1 (int x)
{
  return x < 100;
}

void __attribute__((tainted_args))
test_assert_calling_valid_1 (int n)
{
  MY_ASSERT_1 (__analyzer_valid_1 (n)); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}
 
static int
__analyzer_valid_2 (int x)
{
  return x < 100;
}

void __attribute__((tainted_args))
test_assert_calling_valid_2 (int n)
{
  MY_ASSERT_1 (__analyzer_valid_2 (n)); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}

static int
__analyzer_valid_3 (int x, int y)
{
  if (x >= 100)
    return 0;
  if (y >= 100)
    return 0;
  return 1;
}

void __attribute__((tainted_args))
test_assert_calling_valid_3 (int a, int b)
{
  MY_ASSERT_1 (__analyzer_valid_3 (a, b)); /* { dg-warning "-Wanalyzer-tainted-assertion" "TODO" { xfail *-*-* } } */
}


/* 'switch' statements with supposedly unreachable cases/defaults.  */

int __attribute__((tainted_args))
test_switch_default (int n)
{
  switch (n) /* { dg-message "use of attacker-controlled value for control flow" "why" } */
             /* { dg-message "following 'default:' branch" "dest" { target *-*-* } .-1 } */
    {
    case 0:
      return 5;
    case 1:
      return 22;
    case 2:
      return -1;
    default:
      /* The wording is rather inaccurate here.  */
      __builtin_unreachable (); /* { dg-warning "use of attacked-controlled value in condition for assertion" } */
    }
}

int __attribute__((tainted_args))
test_switch_unhandled_case (int n)
{
  switch (n) /* { dg-message "use of attacker-controlled value for control flow" "why" } */
             /* { dg-message "following 'default:' branch" "dest" { target *-*-* } .-1 } */
    {
    case 0:
      return 5;
    case 1:
      return 22;
    case 2:
      return -1;
    }

  /* The wording is rather inaccurate here.  */
  __builtin_unreachable (); /* { dg-warning "use of attacked-controlled value in condition for assertion" } */
}

int __attribute__((tainted_args))
test_switch_bogus_case_MY_ASSERT_1 (int n)
{
  switch (n)
    {
    default:
    case 0:
      return 5;
    case 1:
      return 22;
    case 2:
      return -1;
    case 42:
      MY_ASSERT_1 (0); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
    }
}

int __attribute__((tainted_args))
test_switch_bogus_case_MY_ASSERT_2 (int n)
{
  switch (n)
    {
    default:
    case 0:
      return 5;
    case 1:
      return 22;
    case 2:
      return -1;
    case 42:
      MY_ASSERT_2 (0); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
    }
}

int __attribute__((tainted_args))
test_switch_bogus_case_unreachable (int n)
{
  switch (n)
    {
    default:
    case 0:
      return 5;
    case 1:
      return 22;
    case 2:
      return -1;
    case 42:
      /* This case gets optimized away before we see it.  */
      __builtin_unreachable ();
    }
}


/* Contents of a struct.  */

struct s
{
  int x;
  int y;
};

int __attribute__((tainted_args))
test_assert_struct (struct s *p)
{
  MY_ASSERT_1 (p->x < p->y); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}
