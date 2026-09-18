/* { dg-do compile } */
/* { dg-options "-march=rv32i_zba -mabi=ilp32 -O2" { target rv32 } } */
/* { dg-options "-march=rv64i_zba -mabi=lp64 -O2" { target rv64 } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

void bar (void *);

/*
** foo:
**	...
**	addi	a0,sp,8
**	call	bar
**	...
*/

void
foo ()
{
  char baz[4104];
  bar (baz);
}
