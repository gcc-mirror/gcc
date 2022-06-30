#include <stdlib.h>
#include <stdio.h>

/* Tests related to statically allocated buffers.  */

typedef struct a {
  short s;
} a;

int *test_1 (void)
{
  a A; /* { dg-message "\\d+ bytes" "note" } */
  A.s = 1;
  int *ptr = (int *) &A; /* { dg-line assign1 } */
  return ptr;

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign1 } */
  /* { dg-message "assigned to 'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } assign1 } */
}

int *test2 (void)
{
  char arr[sizeof (int)];
  int *ptr = (int *)arr;
  return ptr;
}

int *test3 (void)
{
  char arr[sizeof (short)]; /* { dg-message "\\d+ bytes" "note" } */
  int *ptr = (int *)arr; /* { dg-line assign3 } */
  return ptr;

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign3 } */
  /* { dg-message "assigned to 'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } assign3 } */
}
