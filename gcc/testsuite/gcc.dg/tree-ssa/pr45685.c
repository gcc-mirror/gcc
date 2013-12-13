/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-phiopt1-details" } */

typedef unsigned long int uint64_t;
typedef long int int64_t;
int summation_helper_1(int64_t* products, uint64_t count)
{
	int s = 0;
	uint64_t i;
	for(i=0; i<count; i++)
	{	
		int64_t val = (products[i]>0) ? 1 : -1;
		products[i] *= val;
		if(products[i] != i)
			val = -val;
		products[i] = val;
		s += val;
	}
	return s;
}


int summation_helper_2(int64_t* products, uint64_t count)
{
	int s = 0;
	uint64_t i;
	for(i=0; i<count; i++)
	{	
		int val = (products[i]>0) ? 1 : -1;
		products[i] *= val;
		if(products[i] != i)
			val = -val;
		products[i] = val;
		s += val;
	}
	return s;
}

/* { dg-final { scan-tree-dump-times "converted to straightline code" 2 "phiopt1" } } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */

