/* PR91038 */
/* { dg-do run } */
/* { dg-options "-O2 -Wunused-variable" } */


#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

struct lbm {

	int D;
	const int* DQ;

} D2Q9 = { 2,
	(const int*)&(const int[9][2]){
		{ 0, 0 },
		{ 1, 0 }, { 0, 1 }, { -1, 0 }, { 0, -1 },
		{ 1, 1 }, { -1, 1 }, { -1, -1 }, { 1, -1 },
	}
};

void zouhe_left(void)
{
	__auto_type xx = (*({ int N = 2; struct lbm __x = D2Q9; ((const int(*)[9][N])__x.DQ); }));

	if (1 != xx[1][0])
		__builtin_abort();

	if (2 != ARRAY_SIZE(xx[1]))
		__builtin_abort();

	if (1 != (*({ int N = 2; struct lbm __x = D2Q9; ((const int(*)[9][N])__x.DQ); }))[1][0])
		__builtin_abort();

	if (2 != ARRAY_SIZE(*({ int N = 2; struct lbm __x = D2Q9; ((const int(*)[9][N])__x.DQ); })[1]))
		__builtin_abort();
}

int main()
{
	zouhe_left();
	return 0;
}


