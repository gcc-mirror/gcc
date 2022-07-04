#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* Tests related to statically allocated buffers.  */

typedef struct a {
  int16_t s;
} a;

int32_t *test_1 (void)
{
  a A; /* { dg-message "\\d+ bytes" "note" } */
  A.s = 1;
  int32_t *ptr = (int32_t *) &A; /* { dg-line assign1 } */
  return ptr;

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign1 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target *-*-* } assign1 } */
}

int32_t *test2 (void)
{
  char arr[sizeof (int32_t)];
  int32_t *ptr = (int32_t *)arr;
  return ptr;
}

int32_t *test3 (void)
{
  char arr[sizeof (int16_t)]; /* { dg-message "\\d+ bytes" "note" } */
  int32_t *ptr = (int32_t *)arr; /* { dg-line assign3 } */
  return ptr;

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign3 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target *-*-* } assign3 } */
}
