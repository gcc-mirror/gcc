/* { dg-do compile } */
/* { dg-additional-options "-O1 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */
/* { dg-add-options riscv_v } */
/* { dg-final { check-function-bodies "**" "" } } */

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* Check that vector memcpy with predicated store uses smaller LMUL where
   possible.

/* m1
** f1:
**  (
**  vsetivli\s+zero,\d+,e8,m1,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m1,ta,ma
**  )
**  vle8.v\s+v\d+,0\(a1\)
**  vse8.v\s+v\d+,0\(a0\)
**  ret
*/

void f1 (char *d, char *s)
{
  __builtin_memcpy (d, s, MIN_VECTOR_BYTES - 1);
}

/* m2
** f2:
**  (
**  vsetivli\s+zero,\d+,e8,m2,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m2,ta,ma
**  )
**  vle8.v\s+v\d+,0\(a1\)
**  vse8.v\s+v\d+,0\(a0\)
**  ret
*/

void f2 (char *d, char *s)
{
  __builtin_memcpy (d, s, 2 * MIN_VECTOR_BYTES - 1);
}

/* m4
** f3:
**  (
**  vsetivli\s+zero,\d+,e8,m4,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m4,ta,ma
**  )
**  vle8.v\s+v\d+,0\(a1\)
**  vse8.v\s+v\d+,0\(a0\)
**  ret
*/

void f3 (char *d, char *s)
{
  __builtin_memcpy (d, s, 4 * MIN_VECTOR_BYTES - 1);
}

/* m8
** f4:
**  (
**  vsetivli\s+zero,\d+,e8,m8,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m8,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  addi\s+[ta][0-7],[ta][0-7],-?\d+
**  vsetvli\s+zero,[ta][0-7],e8,m8,ta,ma
**  )
**  vle8.v\s+v\d+,0\(a1\)
**  vse8.v\s+v\d+,0\(a0\)
**  ret
*/

void f4 (char *d, char *s)
{
  __builtin_memcpy (d, s, 8 * MIN_VECTOR_BYTES - 1);
}
