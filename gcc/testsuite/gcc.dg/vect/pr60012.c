/* { dg-do compile } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0" } */

typedef struct
{
  short real;
  short imag;
} complex16_t;

void
libvector_AccSquareNorm_ref (unsigned long long  *acc,
			     const complex16_t *x, unsigned len)
{
  unsigned i;
  for (i = 0; i < len; i++)
    acc[i] += ((unsigned long long)((int)x[i].real * x[i].real))
	+ ((unsigned long long)((int)x[i].imag * x[i].imag));
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
