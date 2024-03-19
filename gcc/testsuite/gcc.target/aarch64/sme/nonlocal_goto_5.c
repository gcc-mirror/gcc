/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

void run(void (*)() __arm_streaming);

/*
** bar.0:
**	...
**	smstop	sm
**	br	x[0-9]+
*/
int
foo (int *ptr)
{
  __label__ failure;

  void bar () __arm_streaming { *ptr += 1; goto failure; }
  run (bar);
  return 1;

failure:
  return 0;
}

// { dg-final { scan-assembler-not {smstart\t} } }
// { dg-final { scan-assembler-not {mrs\t} } }
