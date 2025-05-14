/* PR29970, PR91038 */
/* { dg-do run } */
/* { dg-options "-O0 -Wunused-variable" } */

int foo3b(void)   // should not return 0
{
        int n = 0;
        return sizeof *({ n = 10; int x[n]; &x; });
}

int foo4(void)   // should not ICE
{
        return (*({
                        int n = 20;
                        char (*x)[n][n] = __builtin_malloc(n * n);
                        (*x)[12][1] = 1;
                        x;
                }))[12][1];
}

int foo5(void)   // should return 1, returns 0
{
        int n = 0;
        return (*({
                        n = 20;
                        char (*x)[n][n] = __builtin_malloc(n * n);
                        (*x)[12][1] = 1;
                        (*x)[0][1] = 0;
                        x;
                }))[12][1];
}

int foo5c(void)   // should return 400 
{
        int n = 0;
        return sizeof(*({
                        n = 20;
                        char (*x)[n][n] = __builtin_malloc(n * n);
                        (*x)[12][1] = 1;
                        (*x)[0][1] = 0;
                        x;
                }));
}

int foo5b(void)   // should return 1, returns 0
{
	int n = 0;			/* { dg-warning "unused variable" } */
        return (*({
                        int n = 20;
                        char (*x)[n][n] = __builtin_malloc(n * n);
                        (*x)[12][1] = 1;
                        (*x)[0][1] = 0;
                        x;
                }))[12][1];
}

int foo5a(void)   // should return 1, returns 0
{
        return (*({
                        int n = 20;
                        char (*x)[n][n] = __builtin_malloc(n * n);
                        (*x)[12][1] = 1;
                        (*x)[0][1] = 0;
                        x;
                }))[12][1];
}




int main()
{
	if (sizeof(int[10]) != foo3b())
		__builtin_abort();

	if (1 != foo4())
		__builtin_abort();

	if (400 != foo5c())
		__builtin_abort();

	if (1 != foo5a())
		__builtin_abort();

	if (1 != foo5b()) // -O0
		__builtin_abort();

	if (1 != foo5())
		__builtin_abort();

	return 0;
}


