/* { dg-additional-options "-fanalyzer-checker=taint" } */
// TODO: remove need for this option
/* This test can probably be removed when -fanalyzer enables
   the taint checker by default.  */

#include "analyzer-decls.h"

void
test_1 (char a)
{
  char b = -a;
}

/* Copies of code from data-model-1.c.  */

void test_20 (int i, int j)
{
  __analyzer_eval (i + 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i + j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i - 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i - j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i * 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i * j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i / 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i / j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i % 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i % j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i & 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i & j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i | 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i | j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i ^ 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i ^ j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i >> 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i >> j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i << 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i << j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i && 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (i && 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i && j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i || 0); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i || 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (~i); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (-i); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (+i); /* { dg-warning "UNKNOWN" } */

  /* Anything added above should be added to the next function also.  */
}

void test_21 (void)
{
  int i, j, zero;
  int *pi = &i;
  int *pj = &j;
  int *pzero = &zero;
  *pi = 5;
  *pj = 3;
  *pzero = 0;

  __analyzer_eval (i + j == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (i - j == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (i * j == 15); /* { dg-warning "TRUE" } */
  __analyzer_eval (i / j == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i % j == 2); /* { dg-warning "TRUE" } */

  /* Division by zero.  */
  // TODO: should we warn for this?
  __analyzer_eval (i / zero); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i % zero); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval ((i & 1) == (5 & 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i & j) == (5 & 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i | 1) == (5 | 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i | j) == (5 | 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i ^ 1) == (5 ^ 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i ^ j) == (5 ^ 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i >> 1) == (5 >> 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i >> j) == (5 >> 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i << 1) == (5 << 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i << j) == (5 << 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval (i && 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (i && 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i && j); /* { dg-warning "TRUE" } */

  __analyzer_eval (i || 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || j); /* { dg-warning "TRUE" } */

  __analyzer_eval (~i == ~5); /* { dg-warning "TRUE" } */
  __analyzer_eval (-i == -5); /* { dg-warning "TRUE" } */
  __analyzer_eval (+i == +5); /* { dg-warning "TRUE" } */
}
