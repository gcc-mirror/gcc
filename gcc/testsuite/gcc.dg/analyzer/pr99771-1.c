/* Verify that we don't print "<unknown>" in various diagnostics
   (PR analyzer/99771). */

#include <stdlib.h>

void test_1 (void)
{
  *(char*)malloc (1024) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(1024\\)'" } */
} /* { dg-warning "leak of 'malloc\\(1024\\)'" "warning" } */
  /* { dg-message "'malloc\\(1024\\)' leaks here" "final event" { target *-*-* } .-1 } */

void test_2 (size_t n)
{
  *(char*)malloc (4 * n) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(n \\* 4\\)'" "warning" } */
  /* { dg-message "'malloc\\(n \\* 4\\)' could be NULL" "final event" { target *-*-* } .-1 } */
} /* { dg-warning "leak of 'malloc\\(n \\* 4\\)'" "warning" } */
  /* { dg-message "'malloc\\(n \\* 4\\)' leaks here" "final event" { target *-*-* } .-1 } */

/* A compound example.  */

void test_3 (size_t a, size_t b, size_t c)
{
  *(char*)malloc (a + (b * c)) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(a \\+ b \\* c\\)'" "warning" } */
  /* { dg-message "'malloc\\(a \\+ b \\* c\\)' could be NULL" "final event" { target *-*-* } .-1 } */
} /* { dg-warning "leak of 'malloc\\(a \\+ b \\* c\\)'" "warning" } */
  /* { dg-message "'malloc\\(a \\+ b \\* c\\)' leaks here" "final event" { target *-*-* } .-1 } */

void test_4 (size_t a, size_t b, size_t c)
{
  *(char *)malloc (a ? b : c) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(<unknown>\\)'" "warning" } */
  /* { dg-message "'malloc\\(<unknown>\\)' could be NULL" "final event" { target *-*-* } .-1 } */
} /* { dg-warning "leak of 'malloc\\(<unknown>\\)'" "warning" } */
  /* { dg-message "'malloc\\(<unknown>\\)' leaks here" "final event" { target *-*-* } .-1 } */

/* Unary operators.  */

void test_5 (size_t a)
{
  *(char*)malloc (-a) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(-a\\)'" } */
} /* { dg-warning "leak of 'malloc\\(-a\\)'" "warning" } */
  /* { dg-message "'malloc\\(-a\\)' leaks here" "final event" { target *-*-* } .-1 } */

void test_6 (size_t a)
{
  *(char*)malloc (~a) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(~a\\)'" } */
} /* { dg-warning "leak of 'malloc\\(~a\\)'" "warning" } */
  /* { dg-message "'malloc\\(~a\\)' leaks here" "final event" { target *-*-* } .-1 } */

/* Field access.  */

struct s7 { size_t sz; };

void test_7a(struct s7 s)
{
  *(char*)malloc (s.sz) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(s\\.sz\\)'" } */
} /* { dg-warning "leak of 'malloc\\(s\\.sz\\)'" "warning" } */
  /* { dg-message "'malloc\\(s\\.sz\\)' leaks here" "final event" { target *-*-* } .-1 } */

void test_7b (struct s7 *s)
{
  *(char*)malloc (s->sz) = 42; /* { dg-warning "dereference of possibly-NULL 'malloc\\(\\*s\\.sz\\)'" } */
} /* { dg-warning "leak of 'malloc\\(\\*s\\.sz\\)'" "warning" } */
  /* { dg-message "'malloc\\(\\*s\\.sz\\)' leaks here" "final event" { target *-*-* } .-1 } */
