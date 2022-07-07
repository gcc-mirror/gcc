// TODO: remove need for this option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */

#include "analyzer-decls.h"
#include <stdio.h>

struct st1
{
  int a;
  int b;
};


int test_1 (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  return s.a / s.b;  /* { dg-warning "use of attacker-controlled value 's\\.b' as divisor without checking for zero" } */
}

int test_2 (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  return s.a % s.b;  /* { dg-warning "use of attacker-controlled value 's\\.b' as divisor without checking for zero" } */
}

/* We shouldn't complain if the divisor has been checked for zero.  */

int test_checked_ne_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  if (s.b)
    return s.a / s.b; /* { dg-bogus "divisor" } */
  else
    return 0;
}

int test_checked_gt_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  if (s.b > 0)
    return s.a / s.b; /* { dg-bogus "divisor" } */
  else
    return 0;
}

int test_checked_lt_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  if (s.b < 0)
    return s.a / s.b; /* { dg-bogus "divisor" } */
  else
    return 0;
}

/* We should complain if the check on the divisor still allows it to be
   zero.  */

int test_checked_ge_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  if (s.b >= 0)
    return s.a / s.b;  /* { dg-warning "use of attacker-controlled value 's\\.b' as divisor without checking for zero" } */
  else
    return 0;
}

int test_checked_le_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  if (s.b <= 0)
    return s.a / s.b;  /* { dg-warning "use of attacker-controlled value 's\\.b' as divisor without checking for zero" } */
  else
    return 0;
}

int test_checked_eq_zero (FILE *f)
{
  struct st1 s;
  fread (&s, sizeof (s), 1, f);
  /* Wrong sense of test.  */
  if (s.b != 0)
    return 0;
  else
    return s.a / s.b;  /* { dg-warning "use of attacker-controlled value 's\\.b' as divisor without checking for zero" } */
}
