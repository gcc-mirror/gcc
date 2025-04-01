/* { dg-do compile } */
/* { dg-options " -Os -march=rv32i_zca_zcmp -mabi=ilp32 -fno-late-combine-instructions " } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

int
func (int a, int b);

/*
**sum:
**	...
**	cm.mvsa01	s1,s2
**	call	func(?:@plt)?
**	mv	s0,a0
**	cm.mva01s	s1,s2
**	call	func(?:@plt)?
**	...
*/
int
sum (int a, int b)
{
  return func (a, b) + func (a, b);
}
