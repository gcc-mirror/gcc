#include "../../gcc.dg/analyzer/analyzer-decls.h"

void *test_1 (void)
{
  int *p = (int *)__builtin_malloc (sizeof (int));
  if (__builtin_expect (p != NULL, 1))
    {
      *p = 42;
      return p;
    }
  return NULL;    
}

void *test_2 (void)
{
  int *p = (int *)__builtin_malloc (sizeof (int));
  if (__builtin_expect (p == NULL, 1))
    return NULL;

  *p = 42;
  return p;
}

void *test_3 (void)
{
  int *p = (int *)__builtin_malloc (sizeof (int));
  if (__builtin_expect_with_probability (p == NULL, 1, 0.5f))
    return NULL;

  *p = 42;
  return p;
}
