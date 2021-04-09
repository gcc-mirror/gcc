/* { dg-do compile } */

void bar (void (*) (void), int, int);

void
foo (void)
{
  bar (foo, 1, 0);
}

/* Expect assembly like:

	pushl $0
	pushl $1
	pushab foo
	calls $3,bar

rather than:

	clrl -(%sp)
	movl $1,-(%sp)
	movab foo,-(%sp)
	calls $3,bar

 */

/* { dg-final { scan-assembler "\[ \t\]+pushl\[ \t\]+\\\$0\n\[ \t\]+pushl\[ \t\]+\\\$1\n\[ \t\]+pushab\[ \t\]+foo\n" } } */
