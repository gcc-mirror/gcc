/* { dg-do run } */
/* { dg-options "-std=gnu23 -O2" } */

typedef struct E { int a[2]; int b; } *A;

void* foo(void* a, void *b, void *c, void *d)
{
	*(A**)a = c;

	int N = 2;
	typedef struct E { int a[N]; int b; } *B;
	*(B**)b = d;

	return *(A**)a;
}

int main()
{
	A *a, b, c;
	if (&c != (A*)foo(&a, &a, &b, &c))
		__builtin_abort();
}


