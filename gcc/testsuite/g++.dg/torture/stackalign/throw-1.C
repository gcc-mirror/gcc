/* { dg-do run } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT       64
#endif

typedef int t_align __attribute__((aligned(ALIGNMENT)));


int global, global2;
void bar()
{
	volatile t_align a = 1;
        int i,j,k,l,m,n;
        i=j=k=0;
  	for (i=0; i < global; i++)
	  for (j=0; j < i; j++)
	  for (k=0; k < j; k++)
	  for (l=0; l < k; l++)
	  for (m=0; m < l; m++)
	  for (n=0; n < m; n++)
     		global2 = k;
	if (check_int ((int *) &a,  __alignof__(a)) != a)
	  abort ();
	throw 0;
}

void foo()
{
	bar();
}

int main()
{
	int ll = 1;
        int i = 0,j = 1,k = 2,l = 3,m = 4,n = 5;
	try {
  	  for (; i < global; i++)
	  for (; j < i; j++)
	  for (; k < j; k++)
	  for (; l < k; l++)
	  for (; m < l; m++)
	  for (; n < m; n++)
     		global2 = k;
	  foo();
	}
	catch (...)
	{
	}
	ll = i+j+k+l+m+n;
	if (ll != 15)
	{
#ifdef DEBUG
		printf("FAIL: sum %d != 15\n", ll);
#endif
		abort();
	}
	return 0;
}
