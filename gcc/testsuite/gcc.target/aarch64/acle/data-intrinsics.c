/* Test the ACLE data intrinsics.  */
/* { dg-do assemble } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"

/*
** test_clz:
**	clz	w0, w0
**	ret
*/

unsigned int test_clz (uint32_t a)
{
  return __clz (a);
}

/*
** test_clzl:
**	clz	[wx]0, [wx]0
**	ret
*/

unsigned int test_clzl (unsigned long a)
{
  return __clzl (a);
}

/*
** test_clzll:
**	clz	x0, x0
**	ret
*/

unsigned int test_clzll (uint64_t a)
{
  return __clzll (a);
}

/*
** test_cls:
**	cls	w0, w0
**	ret
*/

unsigned int test_cls (uint32_t a)
{
  return __cls (a);
}

/*
** test_clsl:
**	cls	[wx]0, [wx]0
**	ret
*/

unsigned int test_clsl (unsigned long a)
{
  return __clsl (a);
}

/*
** test_clsll:
**	cls	x0, x0
**	ret
*/

unsigned int test_clsll (uint64_t a)
{
  return __clsll (a);
}

/*
** test_rbit:
**	rbit	w0, w0
**	ret
*/

uint32_t test_rbit (uint32_t a)
{
  return __rbit (a);
}

/*
** test_rbitl:
**	rbit	[wx]0, [wx]0
**	ret
*/

unsigned long test_rbitl (unsigned long a)
{
  return __rbitl (a);
}

/*
** test_rbitll:
**	rbit	x0, x0
**	ret
*/

uint64_t test_rbitll (uint64_t a)
{
  return __rbitll (a);
}

/*
** test_rev:
**	rev	w0, w0
**	ret
*/

uint32_t test_rev (uint32_t a)
{
  return __rev (a);
}

/*
** test_revl:
**	rev	[wx]0, [wx]0
**	ret
*/

unsigned long test_revl (unsigned long a)
{
  return __revl (a);
}

/*
** test_revll:
**	rev	x0, x0
**	ret
*/

uint64_t test_revll (uint64_t a)
{
  return __revll (a);
}

/*
** test_rev16:
**	rev16	w0, w0
**	ret
*/

uint32_t test_rev16 (uint32_t a)
{
  return __rev16 (a);
}

/*
** test_rev16l:
**	rev16	[wx]0, [wx]0
**	ret
*/

unsigned long test_rev16l (unsigned long a)
{
  return __rev16l (a);
}

/*
** test_rev16ll:
**	rev16	x0, x0
**	ret
*/

uint64_t test_rev16ll (uint64_t a)
{
  return __rev16ll (a);
}

/*
** test_ror:
**	ror	w0, w0, w1
**	ret
*/

uint32_t test_ror (uint32_t a, uint32_t r)
{
  return __ror (a, r);
}

/*
** test_rorl:
**	ror	[wx]0, [wx]0, [wx]1
**	ret
*/

unsigned long test_rorl (unsigned long a, uint32_t r)
{
  return __rorl (a, r);
}

/*
** test_rorll:
**	ror	x0, x0, x1
**	ret
*/

uint64_t test_rorll (uint64_t a, uint32_t r)
{
  return __rorll (a, r);
}

/*
** test_revsh:
**	rev16	w0, w0
**	ret
*/

int16_t test_revsh (int16_t a)
{
  return __revsh (a);
}

uint32_t *g32;
unsigned long *gul;
uint64_t *g64;
unsigned int *gui;
int16_t *g16;

/*
** test_clz_mem:
**	...
**	clz	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_clz_mem (uint32_t *a)
{
  *gui = __clz (*a);
}

/*
** test_clzl_mem:
**	...
**	clz	[wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_clzl_mem (unsigned long *a)
{
  *gui = __clzl (*a);
}

/*
** test_clzll_mem:
**	...
**	clz	x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_clzll_mem (uint64_t *a)
{
  *gui = __clzll (*a);
}

/*
** test_cls_mem:
**	...
**	cls	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_cls_mem (uint32_t *a)
{
  *gui = __cls (*a);
}

/*
** test_clsl_mem:
**	...
**	cls	[wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_clsl_mem (unsigned long *a)
{
  *gui = __clsl (*a);
}

/*
** test_clsll_mem:
**	...
**	cls	x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_clsll_mem (uint64_t *a)
{
  *gui = __clsll (*a);
}

/*
** test_rbit_mem:
**	...
**	rbit	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_rbit_mem (uint32_t *a)
{
  *g32 = __rbit (*a);
}

/*
** test_rbitl_mem:
**	...
**	rbit	[wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_rbitl_mem (unsigned long *a)
{
  *gul = __rbitl (*a);
}

/*
** test_rbitll_mem:
**	...
**	rbit	x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_rbitll_mem (uint64_t *a)
{
  *g64 = __rbitll (*a);
}

/*
** test_rev_mem:
**	...
**	rev	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_rev_mem (uint32_t *a)
{
  *g32 = __rev (*a);
}

/*
** test_revl_mem:
**	...
**	rev	[wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_revl_mem (unsigned long *a)
{
  *gul = __revl (*a);
}

/*
** test_revll_mem:
**	...
**	rev	x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_revll_mem (uint64_t *a)
{
  *g64 = __revll (*a);
}

/*
** test_rev16_mem:
**	...
**	rev16	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_rev16_mem (uint32_t *a)
{
  *g32 = __rev16 (*a);
}

/*
** test_rev16l_mem:
**	...
**	rev16	[wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_rev16l_mem (unsigned long *a)
{
  *gul = __rev16l (*a);
}

/*
** test_rev16ll_mem:
**	...
**	rev16	x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_rev16ll_mem (uint64_t *a)
{
  *g64 = __rev16ll (*a);
}

/*
** test_ror_mem:
**	...
**	ror	w[0-9]+, w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_ror_mem (uint32_t *a, uint32_t *r)
{
  *g32 = __ror (*a, *r);
}

/*
** test_rorl_mem:
**	...
**	ror	[wx][0-9]+, [wx][0-9]+, [wx][0-9]+
**	...
**	ret
*/

void test_rorl_mem (unsigned long *a, uint32_t *r)
{
  *gul = __rorl (*a, *r);
}

/*
** test_rorll_mem:
**	...
**	ror	x[0-9]+, x[0-9]+, x[0-9]+
**	...
**	ret
*/

void test_rorll_mem (uint64_t *a, uint32_t *r)
{
  *g64 = __rorll (*a, *r);
}

/*
** test_revsh_mem:
**	...
**	rev16	w[0-9]+, w[0-9]+
**	...
**	ret
*/

void test_revsh_mem (int16_t *a)
{
  *g16 = __revsh (*a);
}
