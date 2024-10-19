/* { dg-do compile } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=dynamic" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Tiny memmoves should not be vectorised.
** f1:
**  lbu\s+[ta][0-7],0\(a1\)
**  sb\s+[ta][0-7],0\(a0\)
**  ret
*/
char *
f1 (char *a, char const *b)
{
  return __builtin_memmove (a, b, 1);
}

/* Vectorise+inline minimum vector register width with LMUL=1
** f2:
**  (
**  vsetivli\s+zero,16,e8,m1,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m1,ta,ma
**  )
**  vle8\.v\s+v\d+,0\(a1\)
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
char *
f2 (char *a, char const *b)
{
  return __builtin_memmove (a, b, MIN_VECTOR_BYTES);
}

/* Vectorise+inline up to LMUL=8
** f3:
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m8,ta,ma
**  vle8\.v\s+v\d+,0\(a1\)
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
char *
f3 (char *a, char const *b)
{
  return __builtin_memmove (a, b, MIN_VECTOR_BYTES * 8);
}

/* Don't vectorise if the move is too large for one operation
** f4:
**  li\s+a2,\d+
**  tail\s+memmove
*/
char *
f4 (char *a, char const *b)
{
  return __builtin_memmove (a, b, MIN_VECTOR_BYTES * 8 + 1);
}
