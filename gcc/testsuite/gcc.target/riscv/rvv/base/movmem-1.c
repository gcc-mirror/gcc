/* { dg-do compile } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <string.h>

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen/8)

/* tiny memmoves should not be vectorised
** f1:
**  li\s+a2,15
**  tail\s+memmove
*/
char * f1 (char *a, char const *b)
{
  return memmove (a, b, 15);
}

/* vectorise+inline minimum vector register width with LMUL=1
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
char * f2 (char *a, char const *b)
{
  return memmove (a, b, MIN_VECTOR_BYTES);
}

/* vectorise+inline up to LMUL=8
** f3:
**  li\s+[ta][0-7],\d+
**  vsetvli\s+zero,[ta][0-7],e8,m8,ta,ma
**  vle8\.v\s+v\d+,0\(a1\)
**  vse8\.v\s+v\d+,0\(a0\)
**  ret
*/
char * f3 (char *a, char const *b)
{
  return memmove (a, b, MIN_VECTOR_BYTES*8);
}

/* don't vectorise if the move is too large for one operation
** f4:
**  li\s+a2,\d+
**  tail\s+memmove
*/
char * f4 (char *a, char const *b)
{
  return memmove (a, b, MIN_VECTOR_BYTES*8+1);
}

