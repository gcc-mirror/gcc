/* pr98029 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

double f2 (void)
{
	volatile double d;
	int i;

	for (d = 2.0, i = 0; i < 5; i++, d *= d) /* { dg-bogus "right-hand operand of comma expression has no effect" } */
		;

	return d;
}

int g(void)
{
	volatile int x;
	(x = 1, (void)1); /* { dg-bogus "right-hand operand of comma expression has no effect" } */
	return x;
}
