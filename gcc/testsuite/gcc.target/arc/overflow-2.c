/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdbool.h>
#include <stdint.h>

/*
 * sub.f  r0,r0,r1
 * st_s   r0,[r2]
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool sub_overflow (int32_t a, int32_t b, int32_t *res)
{
  return __builtin_sub_overflow (a, b, res);
}

/*
 * sub.f  r0,r0,-1234
 * st_s   r0,[r1]
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool subi_overflow (int32_t a, int32_t *res)
{
  return __builtin_sub_overflow (a, -1234, res);
}

/*
 * sub.f  r3,r0,r1
 * st_s   r3,[r2]
 * j_s.d  [blink]
 * setlo  r0,r0,r1
 */
bool usub_overflow (uint32_t a, uint32_t b, uint32_t *res)
{
  return __builtin_sub_overflow (a, b, res);
}

/*
 * sub.f  r2,r0,4321
 * seths  r0,4320,r0
 * j_s.d  [blink]
 * st_s   r2,[r1]
 */
bool usubi_overflow (uint32_t a, uint32_t *res)
{
  return __builtin_sub_overflow (a, 4321, res);
}

/*
 * sub.f  r0,r0,r1
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool sub_overflow_p (int32_t a, int32_t b, int32_t res)
{
  return __builtin_sub_overflow_p (a, b, res);
}

/*
 * sub.f  r0,r0,-1000
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool subi_overflow_p (int32_t a, int32_t res)
{
  return __builtin_sub_overflow_p (a, -1000, res);
}

/*
 * j_s.d  [blink]
 * setlo  r0,r0,r1
 */
bool usub_overflow_p (uint32_t a, uint32_t b, uint32_t res)
{
  return __builtin_sub_overflow_p (a, b, res);
}

/*
 * seths  r0,1999,r0
 * j_s.d  [blink]
 */
bool usubi_overflow_p (uint32_t a, uint32_t res)
{
  return __builtin_sub_overflow_p (a, 2000, res);
}

/* { dg-final { scan-assembler-times "sub.f\\s\+"   6 } } */
/* { dg-final { scan-assembler-times "mov\.nv\\s\+" 4 } } */
/* { dg-final { scan-assembler-times "setlo\\s\+"   2 } } */
/* { dg-final { scan-assembler-times "seths\\s\+"   2 } } */
/* { dg-final { scan-assembler-not   "cmp" } } */
