/* { dg-do compile } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=m8" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Vectorise with no loop.
** f1:
**  (
**  vsetivli\s+zero,\d+,e8,m1,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m1,ta,ma
**  )
**  vmv\.v\.x\s+v\d+,a1
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
void *
f1 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES - 1);
}

/* Vectorised code should use smallest lmul known to fit length.
** f2:
**  (
**  vsetivli\s+zero,\d+,e8,m1,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m1,ta,ma
**  )
**  vmv\.v\.x\s+v\d+,a1
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
void *
f2 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES);
}

/* Vectorise+inline operations up to requested lmul.
** f3:
**  (
**  vsetivli\s+zero,\d+,e8,m8,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m8,ta,ma
**  )
**  vmv\.v\.x\s+v\d+,a1
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
void *
f3 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES * 8);
}

/* Don't vectorise if the move is too large for requested lmul.
** f4:
**  li\s+a2,\d+
**  tail\s+memset
*/
void *
f4 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES * 8 + 1);
}
