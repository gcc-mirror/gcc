/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

void f()
{
	goto skip;	/* { dg-warning "jump skips variable initialization" } */
	int i = 1;
skip:	;
}

