/* { dg-do compile }
 * { dg-options "-std=gnu23" } */

// structs with variably modified types

void bar(int n, int m)
{
	struct f { int b; int a[n]; } *x;
	{ struct f { int b; int a[n]; } *x2 = x; }
	{ struct f { int b; int a[m]; } *x2 = x; }
	{ struct f { int b; int a[5]; } *x2 = x; }
	{ struct f { int b; int a[0]; }  *x2 = x; }
	{ struct f { int b; int a[]; }  *x2 = x; }

	struct g { int a[n]; int b; } *y;
	{ struct g { int a[n]; int b; } *y2 = y; }
	{ struct g { int a[m]; int b; } *y2 = y; }
	{ struct g { int a[4]; int b; } *y2 = y; }

	struct h { int b; int a[5]; } *w;
	{ struct h { int b; int a[5]; } *w2 = w; }
	{ struct h { int b; int a[n]; } *w2 = w; }
	{ struct h { int b; int a[m]; } *w2 = w; }

	struct i { int b; int (*a)(int c[n]); } *u;
	{ struct i { int b; int (*a)(int c[4]); } *u2 = u; }
	{ struct i { int b; int (*a)(int c[]); } *u2 = u; }
	{ struct i { int b; int (*a)(int c[*]); } *u2 = u; }
}


