/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** ret_p0:
**	ret
*/
svboolx2_t
ret_p0 (svboolx2_t p0)
{
  return p0;
}

/*
** ret_p1:
**	mov	p0\.b, p1\.b
**	mov	p1\.b, p2\.b
**	ret
*/
svboolx2_t
ret_p1 (svbool_t p0, svboolx2_t p1)
{
  return p1;
}

/*
** ret_p2:
** (
**	mov	p0\.b, p2\.b
**	mov	p1\.b, p3\.b
** |
**	mov	p1\.b, p3\.b
**	mov	p0\.b, p2\.b
** )
**	ret
*/
svboolx2_t
ret_p2 (svboolx2_t p0, svboolx2_t p2)
{
  return p2;
}

/*
** ret_mem:
** (
**	ldr	p0, \[x0\]
**	ldr	p1, \[x0, #1, mul vl\]
** |
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p0, \[x0\]
** )
**	ret
*/
svboolx2_t
ret_mem (svboolx2_t p0, svbool_t p2, svboolx2_t mem)
{
  return mem;
}

/*
** load:
** (
**	ldr	p0, \[x0\]
**	ldr	p1, \[x0, #1, mul vl\]
** |
**	ldr	p1, \[x0, #1, mul vl\]
**	ldr	p0, \[x0\]
** )
**	ret
*/
svboolx2_t
load (svboolx2_t *ptr)
{
  return *ptr;
}

/*
** store:
** (
**	str	p1, \[x0\]
**	str	p2, \[x0, #1, mul vl\]
** |
**	str	p2, \[x0, #1, mul vl\]
**	str	p1, \[x0\]
** )
**	ret
*/
void
store (svbool_t p0, svboolx2_t p1, svboolx2_t *ptr)
{
  *ptr = p1;
}

/*
** upa_p1:
**	ret
*/
void
upa_p1 (svbool_t p0, svboolx2_t p1)
{
  asm volatile ("" :: "Upa" (p1));
}

/*
** up2_p1:
** (
**	mov	p0\.b, p1\.b
**	mov	p1\.b, p2\.b
** |
**	mov	p3\.b, p2\.b
**	mov	p2\.b, p1\.b
** )
**	ret
*/
void
up2_p1 (svbool_t p0, svboolx2_t p1)
{
  asm volatile ("" :: "Up2" (p1));
}

/*
** p1_to_p2:
**	mov	p3\.b, p2\.b
**	mov	p2\.b, p1\.b
**	ret
*/
void
p1_to_p2 (svbool_t p0, svboolx2_t p1)
{
  register svboolx2_t p2 asm ("p2") = p1;
  asm volatile ("" :: "Up2" (p2));
}
