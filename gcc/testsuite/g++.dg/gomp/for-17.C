/* { dg-do compile } */

void foo()
{
	long n = 10;
	int i;
#pragma omp for
	for (i=0; i < n; ++i) ;
#pragma omp for
	for (i=0; n > i; ++i) ;
}
