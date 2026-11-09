/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=scalable -mautovec-preference=sve-only --param=aarch64-autovec-mode=VNx4QI -fdump-tree-vect-details" } */

void
byte_first (unsigned *restrict dst, const unsigned char *restrict src,
	    unsigned bias, unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = (unsigned) src[i] + bias;
}

/* The first data reference selects a full float vector.  The byte load
   must still follow the forced partial-vector policy.  */
void
float_first (float *restrict dst, const float *restrict x,
	     const unsigned char *restrict src, unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = x[i] + (float) src[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Choosing vector mode VNx4QI" 2 "vect" } } */
/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-not {\t[su]unpk} } } */
