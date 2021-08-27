#include "analyzer-decls.h"

#define NULL ((void *)0)

void * __attribute__((noinline))
hide (void *ptr)
{
  return ptr;
}

void test_1 (void)
{
  int a;
  __analyzer_eval (hide (&a) == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (hide (&a) + 1 != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (hide (&a) + 1 == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (hide (&a) - 1 != NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (hide (&a) - 1 == NULL); /* { dg-warning "FALSE" } */
}

void test_2 (void)
{
  __analyzer_eval (hide (NULL) == NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (hide (NULL) - 1 == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (hide (NULL) + 1 == NULL); /* { dg-warning "FALSE" } */
}
