/* { dg-do compile } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fdump-tree-vect-details" } */

void test1(short * __restrict__ x, short * __restrict__ y, short * __restrict__ z)
{
    int i;
    for (i=127; i>=0; i--) {
	x[i] = y[127-i] + z[127-i];
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
