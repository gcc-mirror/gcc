#include <string.h>
#include "analyzer-decls.h"

/* Test GT_EXPR comparison of symbolic values.  */

void test1 (size_t size)
{
  size_t a = 4 * size + 1;
  size_t b = 4 * size;
  __analyzer_eval (a > b); /* { dg-warning "TRUE" } */
}

void test2 (size_t size, size_t offset)
{
  size_t a = size + offset;
  size_t b = size;
  __analyzer_eval (a > b); /* { dg-warning "TRUE" } */
}

void test3 (size_t size, size_t offset)
{
  size_t a = size * offset;
  size_t b = size;
  __analyzer_eval (a > b); /* { dg-warning "TRUE" } */
}

void test4 (size_t size)
{
  size_t op = -1;
  size_t a = size + op;
  size_t b = size;
  __analyzer_eval (a > b); /* { dg-warning "UNKNOWN" } */
}

void test5 (size_t size)
{
  size_t a = size - 1;
  size_t b = size;
  __analyzer_eval (a > b); /* { dg-warning "UNKNOWN" } */
}

void test6 (size_t size, int offset)
{
  /* OFFSET is a symbolic integer, thus could be negative.  */
  size_t a = size + offset;
  size_t b = size;
  __analyzer_eval (a > b); /* { dg-warning "UNKNOWN" } */
}

void test7 (size_t size, size_t mul)
{
  size_t a = mul * size + 1;
  size_t b = mul * size;
  __analyzer_eval (a > b); /* { dg-warning "TRUE" } */
}

void test8 (size_t size)
{
  size_t a = size - 5;
  size_t b = size - 1;
  __analyzer_eval (a > b); /* { dg-warning "UNKNOWN" } */
}

void test9 (size_t size)
{
  size_t a = size + 1;
  size_t b = size + 2;
  __analyzer_eval (a > b); /* { dg-warning "UNKNOWN" } */
}

void test10 (size_t size)
{
  size_t a = size + 2;
  size_t b = size + 1;
  __analyzer_eval (a > b); /* { dg-warning "TRUE" } */
}
