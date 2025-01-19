/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** ret_p0:
**	ret
*/
svboolx4_t
ret_p0 (svboolx4_t p0)
{
  return p0;
}

/*
** ret_p1:
**	addvl	sp, sp, #-1
**	str	p4, \[sp\]
**	mov	p0\.b, p1\.b
**	mov	p1\.b, p2\.b
**	mov	p2\.b, p3\.b
**	mov	p3\.b, p4\.b
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ret
*/
svboolx4_t
ret_p1 (void)
{
  register svboolx4_t p1 asm ("p1");
  asm volatile ("" : "=Upa" (p1));
  return p1;
}

/*
** ret_mem:
** (
**	ldr	p0, \[x0\]
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p2, \[x0, #2, mul vl\]
**	ldr	p3, \[x0, #3, mul vl\]
** |
**	ldr	p3, \[x0, #3, mul vl\]
**	ldr	p2, \[x0, #2, mul vl\]
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p0, \[x0\]
** )
**	ret
*/
svboolx4_t
ret_mem (svboolx4_t p0, svboolx4_t mem)
{
  return mem;
}

/*
** load:
** (
**	ldr	p0, \[x0\]
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p2, \[x0, #2, mul vl\]
**	ldr	p3, \[x0, #3, mul vl\]
** |
**	ldr	p3, \[x0, #2, mul vl\]
**	ldr	p2, \[x0, #3, mul vl\]
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p0, \[x0\]
** )
**	ret
*/
svboolx4_t
load (svboolx4_t *ptr)
{
  return *ptr;
}

/*
** store:
** (
**	str	p0, \[x0\]
**	str	p1, \[x0, #1, mul vl\]
**	str	p2, \[x0, #2, mul vl\]
**	str	p3, \[x0, #3, mul vl\]
** |
**	str	p3, \[x0, #3, mul vl\]
**	str	p2, \[x0, #2, mul vl\]
**	str	p1, \[x0, #1, mul vl\]
**	str	p0, \[x0\]
** )
**	ret
*/
void
store (svboolx4_t p0, svboolx4_t *ptr)
{
  *ptr = p0;
}

/*
** p0_to_p1:
**	addvl	sp, sp, #-1
**	str	p4, \[sp\]
**	mov	p4\.b, p3\.b
**	mov	p3\.b, p2\.b
**	mov	p2\.b, p1\.b
**	mov	p1\.b, p0\.b
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ret
*/
void
p0_to_p1 (svboolx4_t p0)
{
  register svboolx4_t p1 asm ("p1") = p0;
  asm volatile ("" :: "Upa" (p1));
}
