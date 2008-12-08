/* { dg-do compile { target ia64-*-hpux* } } */

extern int foo () __attribute__((version_id ("20040821")));

int bar(int i)
{
	return (foo() + 1);
}

/* { dg-final { scan-assembler "alias.*foo.*foo\\\{20040821\\\}" } } */
