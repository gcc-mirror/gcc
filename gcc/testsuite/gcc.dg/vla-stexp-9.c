/* PR91038 */
/* { dg-do run } */
/* { dg-options "-O2 -Wunused-variable" } */



void foo(void)
{
	if (2 * sizeof(int) != sizeof((*({ int N = 2; int (*x)[9][N] = 0; x; })[1])))
		__builtin_abort();
}

void bar(void)
{
	if (2 * sizeof(int) != sizeof((*({ int N = 2; int (*x)[9][N] = 0; x; })[0])))
		__builtin_abort();
}

void bar0(void)
{
	if (2 * 9 *  sizeof(int) != sizeof((*({ int N = 2; int (*x)[9][N] = 0; x; }))))
		__builtin_abort();
}

void bar11(void)
{
	sizeof(*((*({ int N = 2; int (*x)[9][N] = 0; x; }) + 0)));
}

void bar12(void)
{
	if (2 * sizeof(int) != sizeof(*((*({ int N = 2; int (*x)[9][N] = 0; x; })    ))))
		__builtin_abort();
}

void bar1(void)
{
	if (2 * sizeof(int) != sizeof(*((*({ int N = 2; int (*x)[9][N] = 0; x; }) + 0))))
		__builtin_abort();
}




int main()
{
	foo();
	bar0();
	bar12();
	bar1();
	bar();
}

