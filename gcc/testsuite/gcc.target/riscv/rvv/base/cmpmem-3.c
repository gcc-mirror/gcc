/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=m1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Tiny __builtin_memcmp should use libc.
** f1:
**  li\s+a\d,\d+
**  tail\s+memcmp
*/
int
f1 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES - 1);
}

/* Vectorise+inline minimum vector register width with LMUL=1
** f2:
**  (
**  vsetivli\s+zero,\d+,e8,m1,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m1,ta,ma
**  )
**  ...
**  ret
*/
int
f2 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES);
}

/* Don't inline if the length is too large for one operation.
** f3:
**  li\s+a2,\d+
**  tail\s+memcmp
*/
int
f3 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES + 1);
}
