/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-additional-options "-Ofast -std=gnu99 -fdump-tree-vect-details -fdump-tree-widening_mul" } */

#include <limits.h>
#include <stdint.h>

typedef int16_t elem_t;

__attribute__ ((noipa))
elem_t
foo2 (elem_t *buf, int len)
{
  elem_t x = 0;

  for (int i = 0; i < len; i++)
    x += (elem_t) i * buf[i];

  return x;
}

static elem_t
reference (elem_t *buf, int len)
{
  elem_t x = 0;

#pragma GCC novector
  for (int i = 0; i < len; i++)
    x += (elem_t) i * buf[i];

  return x;
}

int
main (void)
{
  elem_t buf[] = { 1, -2, INT16_MAX, INT16_MIN, 5, -7, 3, -4 };
  int len = sizeof (buf) / sizeof (buf[0]);
  elem_t want = reference (buf, len);
  elem_t got = foo2 (buf, len);

  if (want != got)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "\\.COND_FMA" 1 "widening_mul" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
