/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

#define VECTOR_SIZE 512
typedef float afloat __attribute__ ((__aligned__(16)));

extern void check(const afloat * __restrict__ v);

void square(const afloat * __restrict__ a,
	    afloat * __restrict__ out)
{
    unsigned int i;
    for (i = 0; i < VECTOR_SIZE; i++) {
	float ai = a[i];
	float a2 = ai * ai;
	out[i] = a2; 
    }
    check(out);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"} } */
