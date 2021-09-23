/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-additional-options "-fsanitize=bounds" } */

#include <stdlib.h>
#include "../analyzer-decls.h"

int test_1 (int *arr, int i, int n)
{
  if (i >= n)
    return 0;
  return arr[i];
}

int test_2 (int *arr, int i, int n)
{
  if (i >= n)
    return 0;
  if (arr[i])
    __analyzer_eval (arr[i]); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (arr[i]); /* { dg-warning "FALSE" } */
}

int test_3 (int arr[], int i, int n)
{
  if (i >= n)
    return 0;
  if (arr[i])
    __analyzer_eval (arr[i]); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (arr[i]); /* { dg-warning "FALSE" } */
}

void test_4 (int i, int n)
{
  int arr[n];
  arr[i] = 42;
  __analyzer_eval (arr[i] == 42); /* { dg-warning "TRUE" } */
}

void test_5 (int i, int n)
{
  int *arr = malloc (sizeof(int) * n);
  if (arr)
    {
      arr[i] = 42;
      __analyzer_eval (arr[i] == 42); /* { dg-warning "TRUE" } */
    }
  free (arr);
}

int global;

void test_6 (int i, int n)
{
  int arr[n];
  int saved = global;
  arr[i] = 42;
  __analyzer_eval (saved == global); /* { dg-warning "TRUE" } */
}
