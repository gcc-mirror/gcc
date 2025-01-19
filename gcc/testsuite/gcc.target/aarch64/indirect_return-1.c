/* { dg-do compile } */
/* { dg-options "-O3 -mbranch-protection=bti" } */

int
__attribute((indirect_return,weak))
foo (int a)
{
  return a;
}

/*
**func1:
**	hint	34 // bti c
**	...
**	bl	foo
**	hint	36 // bti j
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
**	hint	34 // bti c
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
**	hint	34 // bti c
**	...
**	bl	func2
**	hint	36 // bti j
**	...
**	ret
*/
int
func3 (int x, int y)
{
  return func2 (x, y);
}

/* { dg-final { check-function-bodies "**" "" "" } } */
