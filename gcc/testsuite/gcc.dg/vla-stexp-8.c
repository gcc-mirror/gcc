/* PR29970, PR91038 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused-variable" } */


int foo0(void)
{
	int c = *(*(*({ int n = 10; int (*x)[n][n] = __builtin_malloc(sizeof *x); x; }) + 5) + 5);
	return c;
}

int foo1(void)
{
	int c = *(5 + *(5 + *({ int n = 10; int (*x)[n][n] = __builtin_malloc(sizeof *x); x; })));
	return c;
}

int bar2(void)
{
	int c = (*({ int n = 10; struct { int y[n]; int z; }* x = __builtin_malloc(sizeof *x); x; })).z;
	return c;
}

int bar3(void)
{
	int n = 2;	/* { dg-warning "unused variable" } */
	int c = (*({ int n = 3; 	/* { dg-warning "unused variable" } */
		({ int n = 10; int (*x)[n][n] = __builtin_malloc(sizeof *x); x; }); }))[5][5];
	return c;
}

int bar3b(void)
{
	int n = 2;	/* { dg-warning "unused variable" } */
	int c = (*({ int n = 3; 	/* { dg-warning "unused variable" } */
		({ int n = 10; int (*x)[n][n] = __builtin_malloc(sizeof *x); x; }); }))[0][0];
	return c;
}

int bar4(void)
{
	int n = 2;	/* { dg-warning "unused variable" } */
	int c = *(5 + *( 5 + *({ int n = 3;	/* { dg-warning "unused variable" } */
		({ int n = 10; int (*x)[n][n] = __builtin_malloc(sizeof *x); x; }); })));
	return c;
}

