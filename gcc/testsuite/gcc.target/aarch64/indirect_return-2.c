/* { dg-do compile } */
/* { dg-options "-O3 -mbranch-protection=none" } */

int
__attribute((indirect_return,weak))
foo (int a)
{
  return a;
}

/*
**func1:
**	...
**	bl	foo
**	...
**	ret
*/
int
func1 (int a, int b)
{
  return foo (a + b);
}

/*
**func2:
**	...
**	b	foo
*/
int __attribute((indirect_return,weak))
func2 (int a, int b)
{
  return foo (a - b);
}

/*
**func3:
**	...
**	bl	func2
**	...
**	ret
*/
int
func3 (int x, int y)
{
  return func2 (x, y);
}

/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler-not {\thint\t} } } */
