/* { dg-do run { target lto } }
 * { dg-options "-std=c23 -O2" }
 */

/* These tests check that definitions of enums with 
 * the same underlying type can alias, even when
 * they are not compatible.  */

enum bar : long { A = 1, B = 3 };

int test_bar(enum bar* a, void* b)
{
	*a = A;

	enum foo : long { C = 2, D = 4 }* p = b;
	*p = B;

	return *a;
}


int main()
{
	enum bar z;

	if (B != test_bar(&z, &z))
		__builtin_abort();

	return 0;
}


