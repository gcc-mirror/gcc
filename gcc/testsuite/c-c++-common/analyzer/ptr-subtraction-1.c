#include <stddef.h>

ptrdiff_t
test_invalid_sub_addrs_of_locals (void)
{
  int a; /* { dg-message "underlying object for left-hand side of subtraction created here" } */
  int b; /* { dg-message "underlying object for right-hand side of subtraction created here" } */
  return &a - &b; /* { dg-warning "undefined behavior when subtracting pointers \\\[CWE-469\\\] \\\[-Wanalyzer-undefined-behavior-ptrdiff\\\]" } */
  /* { dg-message "subtraction of pointers has undefined behavior if they do not point into the same array object" "final event" { target *-*-* } .-1 } */
}

ptrdiff_t
test_valid_sub_addrs_within_array (void)
{
  int a[10];
  return &a[7] - &a[3];
}

ptrdiff_t
test_invalid_sub_addrs_within_arrays (void)
{
  int a[10]; /* { dg-message "left-hand side" } */
  int b[10]; /* { dg-message "right-hand side" } */
  return &a[7] - &b[3]; /* { dg-warning "undefined behavior when subtracting pointers" } */
}


ptrdiff_t
test_invalid_sub_addrs_between_heap_allocs (size_t n)
{
  char *p = (char *)__builtin_malloc (n); /* { dg-message "left-hand side" } */
  char *q = (char *)__builtin_malloc (n); /* { dg-message "right-hand side" } */
  ptrdiff_t d = p - q; /* { dg-warning "undefined behavior when subtracting pointers" } */
  __builtin_free (p);
  __builtin_free (q);
  return d;
}

int arr[42]; /* { dg-message "right-hand side" } */
int sentinel; /* { dg-message "left-hand side" } */

ptrdiff_t
test_invalid_calc_of_array_size (void)
{
  return &sentinel - arr; /* { dg-warning "undefined behavior when subtracting pointers" } */
}
