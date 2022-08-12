#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Wanalyzer-out-of-bounds tests for buffer underreads and writes.  */

/* Avoid folding of memcpy.  */
typedef void * (*memcpy_t) (void *dst, const void *src, size_t n);
static memcpy_t __attribute__((noinline))
get_memcpy (void)
{
  return memcpy;
}


void test1 (void)
{
  int buf[4];
  int *e = buf - 1;
  *e = 42; /* { dg-line test1 } */

  /* { dg-warning "underflow" "warning" { target *-*-* } test1 } */
  /* { dg-message "" "note" { target *-*-* } test1 } */
}

void test2 (void)
{
  int buf[4];
  int *e = buf + 1;
  *e = 123;
  *(e - 1) = 321;
}

void test3 (void)
{
  int buf[4];
  int *e = buf + 1;
  *e = 123;
  *(e - 2) = 321; /* { dg-line test3 } */

  /* { dg-warning "underflow" "warning" { target *-*-* } test3 } */
  /* { dg-message "" "note" { target *-*-* } test3 } */
}

void test4 (void)
{
  memcpy_t fn = get_memcpy ();
  int buf[4];
  memset (buf, 1, 4 * sizeof (int));
  int n = -4;
  fn (&(buf[n]), buf, sizeof (int));  /* { dg-line test4 } */

  /* { dg-warning "underflow" "warning" { target *-*-* } test4 } */
  /* { dg-message "" "note" { target *-*-* } test4 } */
}

void test5 (void)
{
  int buf[4];
  memset (buf, 1, 4 * sizeof (int));

  int sum = 0;
  for (int i = 4; i >= 0; i++)
    sum += *(buf - i); /* { dg-line test5 } */

  /* { dg-warning "underread" "warning" { target *-*-* } test5 } */
  /* { dg-message "" "note" { target *-*-* } test5 } */
}

void test6 (void)
{
  int buf[4];
  memset (buf, 1, 4 * sizeof (int));

  int *view = buf + 1;
  int sum = 0;
  for (int i = 0; i < 4; i++)
    sum += *(view++);
}

void test8 (void)
{
  memcpy_t fn = get_memcpy ();
  int buf[4];
  memset (buf, 1, 4 * sizeof (int));
  int n = -4;
  fn (buf, &(buf[n]), sizeof (int));  /* { dg-line test8 } */

  /* { dg-warning "underread" "warning" { target *-*-* } test8 } */
  /* { dg-message "" "note" { target *-*-* } test8 } */
}
