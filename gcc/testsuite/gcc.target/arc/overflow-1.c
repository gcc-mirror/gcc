/* { dg-do compile } */
/* { dg-options "-O1" } */

#include <stdbool.h>
#include <stdint.h>

/*
 * add.f  r0,r0,r1
 * st_s   r0,[r2]
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool add_overflow (int32_t a, int32_t b, int32_t *res)
{
  return __builtin_add_overflow (a, b, res);
}

/*
 * add.f  r0,r0,-1234
 * st_s   r0,[r1]
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.nv r0,0
 */
bool addi_overflow (int32_t a, int32_t *res)
{
  return __builtin_add_overflow (a, -1234, res);
}

/*
 * add.f  r0,r0,r1
 * st_s   r0,[r2]
 * mov_s  r0,1
 * j_s.d  [blink]
 * mov.hs r0,0
 */
bool uadd_overflow (uint32_t a, uint32_t b, uint32_t *res)
{
  return __builtin_add_overflow (a, b, res);
}

/*
 * add.f  r2,r0, 4321
 * seths  r0,r0,-4321
 * j_s.d  [blink]
 * st_s   r2,[r1]
 */
bool uaddi_overflow (uint32_t a, uint32_t *res)
{
  return __builtin_add_overflow (a, 4321, res);
}

/*
 * add.f   r0,r0,r1
 * mov_s   r0,1
 * j_s.d   [blink]
 * mov.nv  r0,0
 */
bool add_overflow_p (int32_t a, int32_t b, int32_t res)
{
  return __builtin_add_overflow_p (a, b, res);
}

/*
 * add.f   r0,r0,-1000
 * mov_s   r0,1
 * j_s.d   [blink]
 * mov.nv  r0,0
 */
bool addi_overflow_p (int32_t a, int32_t res)
{
  return __builtin_add_overflow_p (a, -1000, res);
}

/*
 * add.f   0,r0,r1
 * mov_s   r0,1
 * j_s.d   [blink]
 * mov.hs  r0,0
 */
bool uadd_overflow_p (uint32_t a, uint32_t b, uint32_t res)
{
  return __builtin_add_overflow_p (a, b, res);
}

/*
 * j_s.d   [blink]
 * seths   r0,r0,-2000
 */
bool uaddi_overflow_p (uint32_t a, uint32_t res)
{
  return __builtin_add_overflow_p (a, 2000, res);
}

/* { dg-final { scan-assembler-times "add.f\\s\+"   7 } } */
/* { dg-final { scan-assembler-times "mov\.nv\\s\+" 4 } } */
/* { dg-final { scan-assembler-times "mov\.hs\\s\+" 2 } } */
/* { dg-final { scan-assembler-times "seths\\s\+"   2 } } */
/* { dg-final { scan-assembler-not   "cmp" } } */
