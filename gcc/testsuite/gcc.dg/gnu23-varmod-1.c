/* { dg-do compile } 
 * { dg-options "-std=gnu23" } */

int foo(int n)
{
	int (*a(void))[n] { return 0; };
	goto err;	/* { dg-error "jump into scope" "variably modified" } */
	typeof((n++,a)) b2;	
err:
	return n;
}

