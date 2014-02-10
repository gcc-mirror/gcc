/* { dg-do compile } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0" } */

typedef struct
{
  short real;
  short imag;
} complex16_t;

void
libvector_AccSquareNorm_ref (unsigned int *acc,
			     const complex16_t *x, unsigned len)
{
  unsigned i;
  for (i = 0; i < len; i++)
    acc[i] += ((unsigned int)((int)x[i].real * x[i].real))
	+ ((unsigned int)((int)x[i].imag * x[i].imag));
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { vect_extract_even_odd } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
