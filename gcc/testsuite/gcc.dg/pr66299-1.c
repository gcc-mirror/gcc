/* PR tree-optimization/66299 */
/* { dg-do run } */
/* { dg-options "-fdump-tree-original" } */
/* { dg-require-effective-target int32plus } */

void
test1 (int x)
{
  if ((0 << x) != 0
      || (1 << x) != 2
      || (2 << x) != 4
      || (3 << x) != 6
      || (4 << x) != 8
      || (5 << x) != 10
      || (6 << x) != 12
      || (7 << x) != 14
      || (8 << x) != 16
      || (9 << x) != 18
      || (10 << x) != 20)
    __builtin_abort ();
}

void
test2 (int x)
{
  if (!((0 << x) == 0
	&& (1 << x) == 4
	&& (2 << x) == 8
	&& (3 << x) == 12
	&& (4 << x) == 16
	&& (5 << x) == 20
	&& (6 << x) == 24
	&& (7 << x) == 28
	&& (8 << x) == 32
	&& (9 << x) == 36
	&& (10 << x) == 40))
    __builtin_abort ();
}

void
test3 (unsigned int x)
{
  if ((0U << x) != 0U
      || (1U << x) != 16U
      || (2U << x) != 32U
      || (3U << x) != 48U
      || (4U << x) != 64U
      || (5U << x) != 80U
      || (6U << x) != 96U
      || (7U << x) != 112U
      || (8U << x) != 128U
      || (9U << x) != 144U
      || (10U << x) != 160U)
    __builtin_abort ();
}

void
test4 (unsigned int x)
{
  if (!((0U << x) == 0U
	|| (1U << x) == 8U
	|| (2U << x) == 16U
	|| (3U << x) == 24U
	|| (4U << x) == 32U
	|| (5U << x) == 40U
	|| (6U << x) == 48U
	|| (7U << x) == 56U
	|| (8U << x) == 64U
	|| (9U << x) == 72U
	|| (10U << x) == 80U))
    __builtin_abort ();
}

void
test5 (int x)
{
  if ((0 << x) == 1
      || (0 << x) != 0
      || (0x8001U << x) != 0x20000U)
    __builtin_abort ();
}

int
main (void)
{
  test1 (1);
  test2 (2);
  test3 (4U);
  test4 (3U);
  test5 (17);
}

/* { dg-final { scan-tree-dump-not "<<" "original" } } */
