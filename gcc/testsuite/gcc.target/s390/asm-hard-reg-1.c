/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test_in_1:
**     foo	%r2
**     br	%r14
*/

int
test_in_1 (int x)
{
  asm ("foo	%0" :: "{r2}" (x));
  return x;
}

/*
** test_in_2:
**     lgr	(%r[0-9]+),%r2
**     lr	%r2,%r3
**     foo	%r2
**     lgr	%r2,\1
**     br	%r14
*/

int
test_in_2 (int x, int y)
{
  asm ("foo	%0" :: "{r2}" (y));
  return x;
}

/*
** test_in_3:
**     stmg	%r12,%r15,96\(%r15\)
**     lay	%r15,-160\(%r15\)
**     lgr	(%r[0-9]+),%r2
**     ahi	%r2,1
**     lgfr	%r2,%r2
**     brasl	%r14,foo@PLT
**     lr	%r3,%r2
**     lr	%r2,\1
**     foo	%r3,%r2
**     lgr	%r2,\1
**     lmg	%r12,%r15,256\(%r15\)
**     br	%r14
*/

extern int foo (int);

int
test_in_3 (int x)
{
  asm ("foo	%0,%1\n" :: "{r3}" (foo (x + 1)), "{r2}" (x));
  return x;
}

/*
** test_out_1:
**     foo	%r3
**     lgfr	%r2,%r3
**     br	%r14
*/

int
test_out_1 (void)
{
  int x;
  asm ("foo	%0" : "={r3}" (x));
  return x;
}

/*
** test_out_2:
**     lgr	(%r[0-9]+),%r2
**     foo	%r2
**     ark	(%r[0-9]+),\1,%r2
**     lgfr	%r2,\2
**     br	%r14
*/

int
test_out_2 (int x)
{
  int y;
  asm ("foo	%0" : "={r2}" (y));
  return x + y;
}

/*
** test_inout_1:
**     foo	%r2
**     lgfr	%r2,%r2
**     br	%r14
*/

int
test_inout_1 (int x)
{
  asm ("foo	%0" : "+{r2}" (x));
  return x;
}
