/* { dg-do compile } */
/* { dg-options " -Os -march=rv32ima_zca_zcmp -mabi=ilp32 -mcmodel=medlow" }*/
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

int callee (int a, int *p);

/*
**test_arg_setup:
**	...
**	li	a0,0
**	call	callee(?:@plt)?
**	cm.popret	{ra}, 32
**	...
*/
int
test_arg_setup (int mode)
{
  int local = mode;
  return callee (0, &local);
}

/*
**test_ret_zero:
**	...
**	call	callee(?:@plt)?
**	cm.popretz	{ra}, 32
**	...
*/
int
test_ret_zero (int mode)
{
  int local = mode;
  callee (0, &local);
  return 0;
}
