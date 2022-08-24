#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* Wanalyzer-out-of-bounds tests for buffer overreads.  */

/* Avoid folding of memcpy.  */
typedef void * (*memcpy_t) (void *dst, const void *src, size_t n);

static memcpy_t __attribute__((noinline))
get_memcpy (void)
{
  return memcpy;
}


void test1 (void)
{
  int id_sequence[3];
  memset (id_sequence, 0, 3 * sizeof(int));
  printf ("%i", id_sequence[3]); /* { dg-line test1 } */

  /* { dg-warning "overread" "warning" { target *-*-* } test1 } */
  /* { dg-message "" "note" { target *-*-* } test1 } */
}

void test2 (void)
{
  int n = 4;
  int arr[n];
  memset (arr, 0, n * sizeof (int));

  int sum = 0;
  for (int i = n - 1; i >= 0; i--)
    sum += arr[i];
}

void test3 (void)
{
  int n = 4;
  int arr[4];
  memset (arr, 0, n * sizeof (int));

  int sum = 0;
  for (int i = n; i > 0; i--)
    sum += arr[i]; /* { dg-line test3 } */

  /* { dg-warning "overread" "warning" { target *-*-* } test3 } */
  /* { dg-message "" "note" { target *-*-* } test3 } */
}

void test4 (void)
{
  int n = 4;
  int *arr = malloc (n * sizeof (int));
  if (!arr)
    return;
  memset (arr, 0, n * sizeof(int));
  
  int sum = 0;
  for (int i = n - 1; i >= 0; i--)
    sum += *(arr + i);

  free (arr);
}

void test5 (void)
{
  int n = 4;
  int *arr = malloc (n * sizeof (int));
  if (!arr)
    return;
  memset (arr, 0, n * sizeof(int));
  
  int sum = 0;
  for (int i = n; i > 0; i--)
    sum += *(arr + i); /* { dg-line test5 } */

  free (arr);
  /* { dg-warning "overread" "warning" { target *-*-* } test5 } */
  /* { dg-message "" "note" { target *-*-* } test5 } */
}
