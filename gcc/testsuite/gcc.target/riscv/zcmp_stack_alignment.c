/* { dg-do compile } */
/* { dg-options " -O0 -march=rv32e_zca_zcb_zcmp -mabi=ilp32e -mcmodel=medlow -fomit-frame-pointer" } */
/* { dg-skip-if "" { *-*-* } {"-O2" "-O1" "-Os" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

void
bar ();

/*
**fool_rv32e:
**	cm.push	{ra}, -32
**	...
**	call	bar(?:@plt)?
**	...
**	lw	a[0-5],32\(sp\)
**	...
**	cm.popret	{ra}, 32
*/
int
fool_rv32e (int a0, int a1, int a2, int a3, int a4, int a5, int incoming0)
{
  bar ();
  return a0 + a1 + a2 + a3 + a4 + a5 + incoming0;
}
