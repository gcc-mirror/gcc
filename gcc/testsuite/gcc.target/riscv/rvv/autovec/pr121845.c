/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0" } */

#include <stdint.h>
typedef uint32_t a;
typedef uint64_t uint64;

uint64 b;
__attribute__ ((__vector_size__ (4 * sizeof (a)))) a f = {504339, 7, 3};
uint64 *g = &b;

int32_t *
c (uint8_t, int32_t *, uint32_t, uint32_t, int64_t);
int8_t
d ()
{
  int32_t e;
  c (0, &e, 0, 0, 1);
  return 0;
}

int32_t *
c (uint8_t, int32_t *j, uint32_t, uint32_t, int64_t)
{
  f = __builtin_shufflevector (f, f, 0, 3, 2, 1);
  *g = f[2];
  return j;
}

int
main ()
{
  d ();
  if (b != 3)
    __builtin_abort ();
}
