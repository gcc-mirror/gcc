/* { dg-do compile } */
/* { dg-options "-Wuseless-cast" } */

void foo(void)
{	
	// casts to the same type
	int i = 0;
	const int ic = 0;
	struct foo { int x; } x = { 0 };
	int q[3];
	(int)ic;		/* { dg-warning "useless cast" } */
	(int)i;			/* { dg-warning "useless cast" } */
	(const int)ic;		/* { dg-warning "useless cast" } */
	(const int)i;		/* { dg-warning "useless cast" } */
	(struct foo)x;		/* { dg-warning "useless cast" } */
	(int(*)[3])&q;		/* { dg-warning "useless cast" } */
	(_Atomic(int))i;	/* { dg-warning "useless cast" } */

	// not the same
	int n = 3;
	(int(*)[n])&q;		// no warning
	int j = (int)0UL;
	enum X { A = 1 } xx = { A };
	enum Y { B = 1 } yy = (enum Y)xx;
}

