/* Test the ACLE data intrinsics get expanded to the correct instructions on a specific architecture  */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_softfp_ok } */
/* { dg-require-effective-target arm_arch_v6_arm_ok } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-add-options arm_arch_v6_arm } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"

uint32_t *g32;
unsigned long *gul;
uint64_t *g64;
unsigned int *gui;
int16_t *g16;

/*
** test_clz:
**	clz	r0, r0
**	bx	lr
*/

unsigned int test_clz (uint32_t a)
{
  return __clz (a);
}

/*
** test_clzl:
**	clz	r0, r0
**	bx	lr
*/

unsigned int test_clzl (unsigned long a)
{
  return __clzl (a);
}

/*
** test_cls:
** eor	(r[0-9]+), r0, r0, asr #31
** clz	(r[0-9]+), \1
** sub	r0, \2, #1
** bx	lr
*/

unsigned int test_cls (uint32_t a)
{
  return __cls(a);
}

/*
** test_clsl:
** eor	(r[0-9]+), r0, r0, asr #31
** clz	(r[0-9]+), \1
** sub	r0, \2, #1
** bx	lr
*/

unsigned int test_clsl (unsigned long a)
{
  return __clsl (a);
}

/*
** test_rev:
**	rev	r0, r0
**	bx	lr
*/

uint32_t test_rev (uint32_t a)
{
  return __rev (a);
}

/*
** test_revl:
**	rev	r0, r0
**	bx	lr
*/

unsigned long test_revl (unsigned long a)
{
  return __revl (a);
}

/*
** test_revll:
**  mov	(r[0-9]+), r0
**  rev	r0, r1
**	rev	r1, \1
**	bx	lr
*/

uint64_t test_revll (uint64_t a)
{
  return __revll (a);
}

/*
** test_ror:
**	and	(r[0-9]+), r1, #31
**	ror	r0, r0, \1
**	bx	lr
*/

uint32_t test_ror (uint32_t a, uint32_t r)
{
  return __ror (a, r);
}

/*
** test_rorl:
**	and	(r[0-9]+), r1, #31
**	ror	r0, r0, \1
**	bx	lr
*/

unsigned long test_rorl (unsigned long a, uint32_t r)
{
  return __rorl (a, r);
}

/*
** test_revsh:
**	revsh	r0, r0
**	bx	lr
*/

int16_t test_revsh (int16_t a)
{
  return __revsh (a);
}

/*
** test_clz_mem:
**	...
**	clz	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_clz_mem (uint32_t *a)
{
  *gui = __clz (*a);
}

/*
** test_clzl_mem:
**	...
**	clz	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_clzl_mem (unsigned long *a)
{
  *gui = __clzl (*a);
}

/*
** test_cls_mem:
**	...
**	clz	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_cls_mem (uint32_t *a)
{
  *gui = __cls (*a);
}

/*
** test_clsl_mem:
**	...
**	clz	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_clsl_mem (unsigned long *a)
{
  *gui = __clsl (*a);
}

/*
** test_rev_mem:
**	...
**	rev	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_rev_mem (uint32_t *a)
{
  *g32 = __rev (*a);
}

/*
** test_revl_mem:
**	...
**	rev	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_revl_mem (unsigned long *a)
{
  *gul = __revl (*a);
}

/*
** test_revll_mem:
**	...
**	rev	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_revll_mem (uint64_t *a)
{
  *g64 = __revll (*a);
}

/*
** test_ror_mem:
**	...
**	ror	r[0-9]+, r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_ror_mem (uint32_t *a, uint32_t *r)
{
  *g32 = __ror (*a, *r);
}

/*
** test_rorl_mem:
**	...
**	ror	r[0-9]+, r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_rorl_mem (unsigned long *a, uint32_t *r)
{
  *gul = __rorl (*a, *r);
}

/*
** test_revsh_mem:
**	...
**	rev16	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_revsh_mem (int16_t *a)
{
  *g16 = __revsh (*a);
}

