/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* PR 116415: Verify our Power8 swap optimization pass doesn't incorrectly swap
   PTImode values.  They should be handled identically to TImode values.  */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef union {
  struct {
    uint64_t a;
    uint64_t b;
  } t;
  __uint128_t data;
} Value;
Value value, next;

void
bug (Value *val, Value *nxt)
{
  for (;;) {
    nxt->t.a = val->t.a + 1;
    nxt->t.b = val->t.b + 2;
    if (__atomic_compare_exchange (&val->data, &val->data, &nxt->data,
				   0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE))
      break;
  }
}

int
main (void)
{
  bug (&value, &next);
  printf ("%lu %lu\n", value.t.a, value.t.b);
  if (value.t.a != 1 || value.t.b != 2)
    abort ();
  return 0;
}
