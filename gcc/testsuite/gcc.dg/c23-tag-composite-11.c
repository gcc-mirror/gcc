/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void f(int n, int m)
{
	typedef struct fo { int a; } aaa[n];
	{
		typedef struct fo { int a; } bbb[m];

	goto foo;			/* { dg-error "jump" } */
		typeof((1 ? (aaa*)0 : (bbb*)0)) x;
	foo:
	}
}

void g(int n, int m)
{
	typedef struct fo { int a; } aaa[n];
	{
		typedef struct fo { int a; } bbb[];

	goto foo;			/* { dg-error "jump" } */
		typeof((1 ? (aaa*)0 : (bbb*)0)) x;
	foo:
	}
}

