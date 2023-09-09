#include <stdlib.h>

/**************************************************************************/

static void maybe_calls_free_1(int *q, int flag)
{
  if (flag)
    free(q); /* { dg-warning "double-'free' of 'q'" } */
}

void test_1(void *p)
{
  maybe_calls_free_1((int *) p, 1);
  maybe_calls_free_1((int *) p, 1); 
}

/**************************************************************************/

static void maybe_calls_free_2(int *q, int flag)
{
  if (flag)
    free(q); /* { dg-bogus "double-'free'" } */
}

void test_2(void *p)
{
  maybe_calls_free_2((int *) p, 0);
  maybe_calls_free_2((int *) p, 0);
}
