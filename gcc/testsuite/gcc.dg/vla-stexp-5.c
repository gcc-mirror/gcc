/* PR29970 */
/* { dg-do run } */
/* { dg-options "-Wunused-variable" } */
/* { dg-require-effective-target alloca } */




int foo2a(void)   // should not ICE
{
        return ({ int n = 20; struct { int x[n];} x; x.x[12] = 1; sizeof(x); });
}


int foo2b(void)   // should not ICE
{
        return sizeof *({ int n = 20; struct { int x[n];} x; x.x[12] = 1; &x; });
}

int main()
{
	if (sizeof(struct { int x[20]; }) != foo2a())
		__builtin_abort();

	if (sizeof(struct { int x[20]; }) != foo2b())
		__builtin_abort();

	return 0;
}


