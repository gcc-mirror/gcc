/* Test the ACLE data intrinsics existence for specific instruction.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v6t2_hw } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-add-options arm_arch_v6t2 } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"

extern void abort (void);

uint32_t *g32;
unsigned long *gul;
uint64_t *g64;

/*
** test_rbit:
**	rbit	r0, r0
**	bx	lr
*/

uint32_t test_rbit (uint32_t a)
{
  return __rbit (a);
}

/*
** test_rbitl:
**	rbit	r0, r0
**	bx	lr
*/

unsigned long test_rbitl (unsigned long a)
{
  return __rbitl (a);
}

/*
** test_rbitll:
** mov	(r[0-9]+), r0
** rbit	r0, r1
** rbit	r1, \1
** bx	lr
*/

uint64_t test_rbitll (uint64_t a)
{
  return __rbitll (a);
}

/*
** test_rbit_mem:
**	...
**	rbit	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_rbit_mem (uint32_t *a)
{
  *g32 = __rbit (*a);
}

/*
** test_rbitl_mem:
**	...
**	rbit	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_rbitl_mem (unsigned long *a)
{
  *gul = __rbitl (*a);
}

/*
** test_rbitll_mem:
**	...
**	rbit	r[0-9]+, r[0-9]+
**	...
**	bx	lr
*/

void test_rbitll_mem (uint64_t *a)
{
  *g64 = __rbitll (*a);
}

int
main (int argc, char **argv)
{
  if (__rbit(0x12345678) != 0x1e6a2c48) { abort(); }
  if (__rbitl(0x12345678) != 0x1e6a2c48) { abort(); }
  if (__rbitll(0x1234567890abcdef) != 0xf7b3d5091e6a2c48) { abort(); }
  return 0;
}

