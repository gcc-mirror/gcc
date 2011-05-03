#include <stdlib.h>

int t[] = { 1, 2, 3, 4, 5, 6, 7 };
int expect[] = { 1, 3, 6, 10, 15, 21, 28, 29, 31, 34, 38, 43, 49, 56 };

int foo (int n)
{
    int *p = t;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++) {
	sum += *p;
	p = __builtin_bfin_circptr (p, sizeof *p, t, sizeof t);
    }
    return sum;
}

int main ()
{
    int i;
    int *p = expect;
    for (i = 0; i < 14; i++) {
	int sum = foo (i + 1);
	if (sum != *p)
	    abort ();
	p = __builtin_bfin_circptr (p, sizeof *p, expect, sizeof expect);
    }
    return 0;
}
