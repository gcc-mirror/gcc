/* { dg-do compile { target bitint } } */
/* { dg-additional-options "-march=rv64gc -mabi=lp64d -std=c23 -O -fno-stack-clash-protection -g -fpie -mcmodel=medlow -fno-section-anchors" } */
/* { dg-skip-if "" { *-*-* } { "-flto" "-O0"} } */
/* { dg-final { check-function-bodies "**" "" } } */

signed _BitInt(32) a;
unsigned _BitInt(32) b;
signed _BitInt(16) c;
unsigned _BitInt(16) d;

signed _BitInt(32) f1(void) {
/*
** f1:
**	...
**	lw	a0,a
**      ret
*/
  return a;
}
unsigned _BitInt(32) f2(void) {
/*
** f2:
**	...
**	lw	a0,b
**      ret
*/
  return b;
}

signed _BitInt(16) f3(void) {
/*
** f3:
**	...
**	lh	a0,c
**      ret
*/
  return c;
}
unsigned _BitInt(16) f4(void) {
/*
** f4:
**	...
**	lhu	a0,d
**      ret
*/
  return d;
}
