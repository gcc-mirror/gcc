#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;
extern void bzero (void *s, size_t n);
extern void *memset(void *s, int c, size_t n);

void test_1 (void)
{
  char arr[16];
  bzero (arr, sizeof (arr));
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[7] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[8] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[9] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */

  /* Clobber in the middle (with prefix and suffix).  */
  arr[8] = 42;
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[7] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[8] == 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (arr[8] == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[9] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */
}

void test_2 (void)
{
  char arr[16];
  bzero (arr, sizeof (arr));
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[1] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */

  /* Clobber at the front (suffix, but no prefix).  */
  arr[0] = 42;
  __analyzer_eval (arr[0] == 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (arr[0] == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[1] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */
}

void test_3 (void)
{
  char arr[16];
  bzero (arr, sizeof (arr));
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[14] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */

  /* Clobber at the end (prefix, but no suffix).  */
  arr[15] = 42;
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[14] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (arr[15] == 42); /* { dg-warning "TRUE" } */
}

void test_4 (void)
{
  char arr[16];
  bzero (arr, sizeof (arr));
  __analyzer_eval (arr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "TRUE" } */

  /* Exact overlap, no prefix or suffix.  */
  memset (arr, 1, 16);
  __analyzer_eval (arr[0] == 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (arr[15] == 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (arr[0] == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[15] == 1); /* { dg-warning "TRUE" } */
}
