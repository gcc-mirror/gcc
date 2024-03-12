#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

/* Wanalyzer-out-of-bounds tests for buffer overflows.  */

/* Avoid folding of memcpy.  */
typedef void * (*memcpy_t) (void *dst, const void *src, size_t n);

static memcpy_t __attribute__((noinline))
get_memcpy (void)
{
  return memcpy;
}


/* Taken from CWE-787.  */
void test1 (void)
{
  int id_sequence[3];

  id_sequence[0] = 123;
  id_sequence[1] = 234;
  id_sequence[2] = 345;
  id_sequence[3] = 456; /* { dg-line test1 } */

  /* { dg-warning "stack-based buffer overflow" "warning" { target *-*-* } test1 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'id_sequence'" "num bad bytes note" { target *-*-* } test1 } */
  /* { dg-message "valid subscripts for 'id_sequence' are '\\\[0\\\]' to '\\\[2\\\]'" "valid subscript note" { target *-*-* } test1 } */
}

void test2 (void)
{
  int n = 4;
  int arr[4];

  for (int i = n - 1; i >= 0; i--)
    arr[i] = i;
}

void test3 (void)
{
  int n = 4;
  int arr[4];

  for (int i = n; i >= 0; i--)
    arr[i] = i; /* { dg-line test3 } */

  /* { dg-warning "stack-based buffer overflow" "warning" { target *-*-* } test3 } */
  /* { dg-message "write of 4 bytes to beyond the end of 'arr'" "num bad bytes note" { target *-*-* } test3 } */
  /* { dg-message "valid subscripts for 'arr' are '\\\[0\\\]' to '\\\[3\\\]'" "valid subscript note" { target *-*-* } test3 } */
}

void test4 (void)
{
  int *arr = (int *)malloc (4 * sizeof (int));
  if (!arr)
    return;
  
  int *last_el = arr + 3;
  *last_el = 3;

  free (arr);
}

void test5 (void)
{
  int *arr = (int *)malloc (4 * sizeof (int));
  if (!arr)
    return;
  
  int *last_el = arr + 4;
  *last_el = 4; /* { dg-line test5 } */

  free (arr);
  /* { dg-warning "heap-based buffer overflow" "warning" { target *-*-* } test5 } */
  /* { dg-message "" "note" { target *-*-* } test5 } */
}

/* Taken from "A Provenance-aware Memory Object Model for C".  */
int y = 2, x = 1; /* { dg-message "capacity" } */
void test6 (void)
{
  int *p = &x + 1;
  int *q = &y;
  printf ("Addresses: p=% p q=% p \n" , (void *) p, (void *) q);
  if (memcmp (&p , &q , sizeof (p)) == 0)
  {
    *p = 11; /* { dg-line test6b } */
    printf ("x=%d y=%d *p=%d *q=%d\n" , x, y, *p, *q);  /* { dg-line test6c } */
  }

  /* { dg-warning "buffer overflow" "warning" { target *-*-* } test6b } */
  /* { dg-message "" "note" { target *-*-* } test6b } */
  /* { dg-warning "buffer over-read" "warning" { target *-*-* } test6c } */
  /* { dg-message "" "note" { target *-*-* } test6c } */
}

extern int is_valid (void);

int returnChunkSize (int *ptr)
{
  /* If chunk info is valid, return the size of usable memory,
     else, return -1 to indicate an error.  */
  return is_valid () ? sizeof (*ptr) : -1;
}

/* Taken from CWE-787.  */
void test7 (void)
{
  memcpy_t fn = get_memcpy ();

  int destBuf[4];
  int srcBuf[4];
  fn (destBuf, srcBuf, returnChunkSize (destBuf)); /* { dg-line test7 } */

  // TODO: Should we handle widening_svalues as a follow-up?
  /* { dg-warning "over-read" "warning" { xfail *-*-* } test7 } */
  /* { dg-warning "use of uninitialized value" "uninit warning" { target *-*-* } test7 } */
  /* { dg-warning "overflow" "warning" { xfail *-*-* } test7 } */
}
