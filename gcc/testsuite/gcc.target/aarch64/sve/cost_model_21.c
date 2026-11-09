/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-v3 -mautovec-preference=sve-only -msve-vector-bits=scalable -fdump-tree-vect-details" } */

/* Widening through several element sizes creates dependent unpack stages.
   Prefer widening loads to a chain of unpack instructions.  */
void
widen_add (unsigned long *restrict dst, const unsigned char *restrict src,
	   unsigned bias, unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    dst[i] = (unsigned) src[i] + bias;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-assembler-not {\tuunpk(lo|hi)\t} } } */
