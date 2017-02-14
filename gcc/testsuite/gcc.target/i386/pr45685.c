/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mno-sse4 -ftree-loop-if-convert" } */

typedef unsigned long long int uint64_t;
typedef long long int int64_t;
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

/* { dg-final { scan-assembler-times "cmov" 6 } } */
