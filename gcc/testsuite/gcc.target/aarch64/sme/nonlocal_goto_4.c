/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

void run(void (*)());

/*
** bar.0:
**	...
**	smstart	sm
**	...
**	smstop	sm
**	br	x[0-9]+
*/
int
foo (int *ptr)
{
  __label__ failure;

  __arm_locally_streaming void bar () { *ptr += 1; goto failure; }
  run (bar);
  return 1;

failure:
  return 0;
}
