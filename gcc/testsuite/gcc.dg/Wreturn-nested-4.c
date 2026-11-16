/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "" } */

void *foo()
{
	int bar(int x)
	{
		return x ? bar(x - 1) * x : 1;
	}

	return bar;	/* no warning */
}
