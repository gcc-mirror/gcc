/* Tests for labels before declarations and at ends of compound statements.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

int f(int x) 
{ 
	goto b;
	a: int i = 2 * x;
           goto c;
	b: goto a;
	{ i *= 3; c: }
	return i;
        d:
}

