/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#ifndef TYPE
#define TYPE svint8_t
#define DUPQ svdupq_lane_s8
#define INDEX svindex_s8
#define COUNT 16
#endif

#define BASE 42

TYPE __attribute__ ((noipa))
dupq_var (TYPE x, uint64_t y)
{
  return DUPQ (x, y);
}

TYPE __attribute__ ((noipa))
dupq_0 (TYPE x)
{
  return DUPQ (x, 0);
}

TYPE __attribute__ ((noipa))
dupq_1 (TYPE x)
{
  return DUPQ (x, 1);
}

TYPE __attribute__ ((noipa))
dupq_2 (TYPE x)
{
  return DUPQ (x, 2);
}

TYPE __attribute__ ((noipa))
dupq_3 (TYPE x)
{
  return DUPQ (x, 3);
}

TYPE __attribute__ ((noipa))
dupq_4 (TYPE x)
{
  return DUPQ (x, 4);
}

void __attribute__ ((noipa))
check (TYPE x, uint64_t y)
{
  svbool_t pg = svptrue_b8 ();
  if (y * 2 >= svcntd ())
    {
      if (svptest_any (pg, svcmpne (pg, x, 0)))
	__builtin_abort ();
    }
  else
    {
      TYPE repeat = svand_x (pg, INDEX (0, 1), COUNT - 1);
      TYPE expected = svadd_x (pg, repeat, BASE + y * COUNT);
      if (svptest_any (pg, svcmpne (pg, x, expected)))
	__builtin_abort ();
    }
}

int
main ()
{
  TYPE x = INDEX (BASE, 1);

  check (dupq_0 (x), 0);
  check (dupq_1 (x), 1);
  check (dupq_2 (x), 2);
  check (dupq_3 (x), 3);
  check (dupq_4 (x), 4);

  for (int i = 0; i < 63; ++i)
    {
      check (dupq_var (x, i), i);
      check (dupq_var (x, (uint64_t) 1 << i), (uint64_t) 1 << i);
    }

  return 0;
}
