/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flto" } */

typedef int *A;

void* foo(void* a, void *b, void *c, void *d)
{
	*(A**)a = c;

	typedef enum E *B;
	enum E { E1 = -1, E2 = 0, E3 = 1 };
	{
		*(B**)b = d;
	}

	return *(A**)a;
}

int main()
{
	A *a, b, c;
	if (&c != (A*)foo(&a, &a, &b, &c))
		__builtin_abort();
}


