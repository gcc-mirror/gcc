/* { dg-do "run" } */
/* { dg-options "-std=c2y" } */

int main()
{
	int n = 1;
	int a[1];
	_Generic(typeof(a), int[n++]: 0);
	float b[1];
	_Generic(typeof(b), int[n++]: 0, default: 1);
	if (n != 1)
		__builtin_abort();
	return 0;
}

