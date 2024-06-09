/* { dg-additional-options "-Wno-placement-new -Wno-analyzer-use-of-uninitialized-value" } */

#include <new>
#include <stdlib.h>
#include <stdint.h>

extern int get_buf_size ();

void var_too_short ()
{
  int8_t s;
  int64_t *lp = new (&s) int64_t; /* { dg-warning "stack-based buffer overflow" } */
  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" "" { target *-*-* } .-1 } */
}

void static_buffer_too_short ()
{
  int n = 16;
  int buf[n];
  int *p = new (buf) int[n + 1]; /* { dg-warning "stack-based buffer overflow" } */
}

void symbolic_buffer_too_short ()
{
  int n = get_buf_size ();
  char buf[n];
  char *p = new (buf) char[n + 10]; /* { dg-warning "stack-based buffer overflow" } */
}

void test_binop ()
{
  char *p = (char *) malloc (4);
  if (!p)
    return;
  int32_t *i = ::new (p + 1) int32_t; /* { dg-warning "heap-based buffer overflow" } */
  *i = 42; /* { dg-warning "heap-based buffer overflow" } */
  free (p);
}
