/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=m1" } */
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

/* Vectorise+inline minimum vector register width using requested lmul.
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

/* Don't vectorise if the move is too large for requested lmul.
** f3:
**  li\s+a2,\d+
**  tail\s+memset
*/
void *
f3 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES + 1);
}
