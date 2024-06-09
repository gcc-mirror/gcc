/* { dg-do compile }
 * { dg-options "-std=c23" } */

void f(int n)
{
	int a[n];
	goto foo;	/* { dg-error "jump into scope" "variably modified" } */
	typeof(a) b1;		
foo:
}

void g(int n)
{
	int a2[1][n];
	goto foo;	/* { dg-error "jump into scope" "variably modified" } */
	typeof((n++,a2)) b2;
foo:
}

void h(int n)
{
	int a[n];
	typeof(a) b1;		
	goto foo;	/* { dg-error "jump into scope" "variably modified" } */
	typeof(&b1) b;
foo:
}
