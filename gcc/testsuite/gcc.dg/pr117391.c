/* { dg-do compile } */
/* { dg-options "-std=c99" } */

int foo(int n, char (*buf)[*]);
int bar(int n, char (*buf)[n]);

void test()
{
	(1 ? foo : bar)(0);	/* { dg-error "too few arguments to function '\\\(int \\\(\\\*\\\)\\\(int,  char \\\(\\\*\\\)\\\[n]\\\)\\\)&foo'" } */
	(0 ? bar : foo)(0);	/* { dg-error "too few arguments to function '\\\(int \\\(\\\*\\\)\\\(int,  char \\\(\\\*\\\)\\\[n]\\\)\\\)&foo'" } */
	(0 ? foo : bar)(0);	/* { dg-error "too few arguments to function 'bar'" } */
	(1 ? bar : foo)(0);	/* { dg-error "too few arguments to function 'bar'" } */
}

