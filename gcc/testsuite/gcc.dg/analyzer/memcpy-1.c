#include <string.h>
#include "analyzer-decls.h"

void *test_1 (void *dst, void *src, size_t n)
{
  void *result = memcpy (dst, src, n);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

void *test_1a (void *dst, void *src, size_t n)
{
  void *result = __memcpy_chk (dst, src, n, -1);
  __analyzer_eval (result == dst); /* { dg-warning "TRUE" } */
  return result;
}

void test_2 (int i)
{
  int j;
  memcpy (&j, &i, sizeof (int));
  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
}

void test_2a (int i)
{
  int j;
  __memcpy_chk (&j, &i, sizeof (int), sizeof (int));
  __analyzer_eval (i == j);  /* { dg-warning "TRUE" } */
}

void test_3 (void *src, size_t n)
{
  char buf[40], other[40];
  buf[0] = 'a';
  other[0] = 'b';
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "TRUE" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */

  memcpy (buf, src, n);
  __analyzer_eval (buf[0] == 'a');    /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (other[0] == 'b');  /* { dg-warning "TRUE" } */
}
