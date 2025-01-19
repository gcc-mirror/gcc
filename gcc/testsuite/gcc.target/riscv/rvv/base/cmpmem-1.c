/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=dynamic" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Trivial memcmp should use inline scalar ops.
** f1:
**  lbu\s+a\d+,0\(a0\)
**  lbu\s+a\d+,0\(a1\)
**  subw?\s+a0,a\d+,a\d+
**  ret
*/
int
f1 (void *a, void *b)
{
  return __builtin_memcmp (a, b, 1);
}

/* Tiny __builtin_memcmp should use libc.
** f2:
**  li\s+a\d,\d+
**  tail\s+memcmp
*/
int
f2 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES - 1);
}

/* Vectorise+inline minimum vector register width with LMUL=1
** f3:
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
f3 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES);
}

/* Vectorised code should use smallest lmul known to fit length
** f4:
**  (
**  vsetivli\s+zero,\d+,e8,m2,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m2,ta,ma
**  )
**  ...
**  ret
*/
int
f4 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES + 1);
}

/* Vectorise+inline up to LMUL=8
** f5:
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m8,ta,ma
**  ...
**  ret
*/
int
f5 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES * 8);
}

/* Don't inline if the length is too large for one operation.
** f6:
**  li\s+a2,\d+
**  tail\s+memcmp
*/
int
f6 (void *a, void *b)
{
  return __builtin_memcmp (a, b, MIN_VECTOR_BYTES * 8 + 1);
}
