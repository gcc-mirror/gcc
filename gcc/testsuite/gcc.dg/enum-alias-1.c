/* { dg-do run } */
/* { dg-options "-O2" } */

enum E { E1 = -1, E2 = 0, E3 = 1, MAX = __INT_MAX__ };

typedef int A;
typedef enum E B;

_Static_assert(_Generic((A){ 0 }, B: 1), "");

void* foo(void* a, void *b, A *c, B *d)
{
	*(A**)a = c;
	*(B**)b = d;
	return *(A**)a;
}

int main()
{
	A *a, b, c;
	if (&c != (A*)foo(&a, &a, &b, &c))
		__builtin_abort();
}

