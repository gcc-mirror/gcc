/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-tree-sra" } */

/* Even without SRA being enabled, we should be able to eliminate every structure store and load here. */
extern void foo (const int);
int main(void)
{
	struct a
	{
		int e;
		int f;
		int g;
	}  a;
	struct a b;
	int x, c;
	a.e = 50;
	a.f = 9;
	a.g = a.e * a.f;
	foo (a.f);
	foo (a.g);
	x = a.f;
	c = a.e;
	foo (x);
	foo (c);
	a.e = 5;
	a.f = 40;
	a.g = 90;
	foo (a.e);
	foo (a.f);
	foo (a.g);
	c = a.f;
	foo (c);
	b.e = 9;
	a.e = b.e + 1 * c;
	a.f = 30;
	foo (a.e);
	foo (a.f);
	x = a.e * a.f;
	foo (x);

}
/* { dg-final { scan-tree-dump-times "a.e" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "a.f" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "a.g" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "b.e" 0 "optimized" } } */
