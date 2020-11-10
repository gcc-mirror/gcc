/* Tests for labels before declarations and at ends of compound statements.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int f(int x) 
{ 
	goto b;
	a: int i = 2 * x;	/* { dg-error "a label can only be part of a statement and a declaration is not a statement" } */
           goto c;
	b: goto a;
	{ i *= 3; c: }		/* { dg-error "label at end of compound statement" } */
	return i;
        d:			/* { dg-error "label at end of compound statement" } */
}

