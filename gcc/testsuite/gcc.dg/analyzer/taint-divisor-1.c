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
