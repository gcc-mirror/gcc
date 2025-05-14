/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=dynamic" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Tiny memsets should use scalar ops.
** f1:
**  sb\s+a1,0\(a0\)
**  ret
*/
void *
f1 (void *a, int const b)
{
  return __builtin_memset (a, b, 1);
}

/* Tiny memsets should use scalar ops.
** f2:
**  sb\s+a1,0\(a0\)
**  sb\s+a1,1\(a0\)
**  ret
*/
void *
f2 (void *a, int const b)
{
  return __builtin_memset (a, b, 2);
}

/* Tiny memsets should use scalar ops.
** f3:
**  sb\s+a1,0\(a0\)
**  sb\s+a1,1\(a0\)
**  sb\s+a1,2\(a0\)
**  ret
*/
void *
f3 (void *a, int const b)
{
  return __builtin_memset (a, b, 3);
}

/* Vectorise+inline minimum vector register width with LMUL=1
** f4:
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
f4 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES);
}

/* Vectorised code should use smallest lmul known to fit length
** f5:
**  (
**  vsetivli\s+zero,\d+,e8,m2,ta,ma
**  |
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m2,ta,ma
**  )
**  vmv\.v\.x\s+v\d+,a1
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
void *
f5 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES + 1);
}

/* Vectorise+inline up to LMUL=8
** f6:
**  li\s+a\d+,\d+
**  vsetvli\s+zero,a\d+,e8,m8,ta,ma
**  vmv\.v\.x\s+v\d+,a1
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
void *
f6 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES * 8);
}

/* Don't vectorise if the move is too large for one operation.
** f7:
**  li\s+a2,\d+
**  tail\s+memset(?:@plt)?
*/
void *
f7 (void *a, int const b)
{
  return __builtin_memset (a, b, MIN_VECTOR_BYTES * 8 + 1);
}
