/* Tests for labels before declarations and at ends of compound statements
 * in combination with attributes. */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wall" } */

int f(void) 
{ 
	goto b;
	a: int i = 0;
	aa: __attribute__((unused)) int u = 0; int v = 0;	/* { dg-warning "GNU-style attribute between label and declaration appertains to the label" } */
           goto c;
	{ c: }
	b: goto a;
	return i + u + v;
        d: __attribute__((unused)) (void)0;
        e: __attribute__((unused))
}

int g(void) 
{ 
	goto b;
	a: int i = 0;
	[[maybe_unused]] aa: int u = 0; int v = 0;
           goto c;
	{ c: }
	b: goto a;
	return i + u + v;
        [[maybe_unused]] d: (void)0;
        [[maybe_unused]] e:
}

void h(void)
{
	[[maybe_unused]] a: [[maybe_unused]] b: [[maybe_unused]] int x;

	if (1)
		[[maybe_unused]] c: [[maybe_unused]] d: (void)0;
}
