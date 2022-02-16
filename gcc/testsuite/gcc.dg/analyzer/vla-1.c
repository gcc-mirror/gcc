/* { dg-require-effective-target alloca } */
#include "analyzer-decls.h"

void test_1 (int n)
{
  struct
  {
    char a[n], b;
  } s;
  s.a[0] = 42;
  __analyzer_eval (s.a[0] == 42); /* { dg-warning "TRUE" } */
  s.b = 17;
  __analyzer_eval (s.b == 17); /* { dg-warning "TRUE" } */
}

void test_2 (int n)
{
  int arr[n]; /* { dg-message "region created on stack here" } */
  __builtin_free (arr); /* { dg-warning "'free' of '<unknown>' which points to memory on the stack" } */
  // TODO: fix the "unknown" here
}

/* { dg-prune-output "\\\[-Wfree-nonheap-object" } */
