/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=ampere1 -fdump-tree-vect-details" } */

/* The inner dot product accumulates through USDOT, whose 3-cycle
   accumulator latency on ampere1 makes the vectorizer unroll the
   reduction into four independent accumulators: 64 bytes per iteration
   with 16-byte vectors.  */

#define IN_DIM 512
#define OUT_DIM 32

void
affine (int *__restrict out, const signed char *weights,
	const int *biases, const unsigned char *input)
{
  for (int i = 0; i < OUT_DIM; ++i)
    {
      int s = biases[i];
      for (int j = 0; j < IN_DIM; ++j)
	s += (int) weights[i * IN_DIM + j] * (int) input[j];
      out[i] = s;
    }
}

/* These loops are too short to unroll by four.  They keep 16-byte vectors
   and their dot products: one SDOT in dot16, two UDOTs in sum32 and one in
   sad16.  */

signed char sa[16], sb[16];
unsigned char ua[32], ub[32];

int
dot16 (void)
{
  int s = 0;
  for (int i = 0; i < 16; ++i)
    s += sa[i] * sb[i];
  return s;
}

int
sum32 (void)
{
  int s = 0;
  for (int i = 0; i < 32; ++i)
    s += ua[i];
  return s;
}

int
sad16 (void)
{
  int s = 0;
  for (int i = 0; i < 16; ++i)
    s += __builtin_abs (ua[i] - ub[i]);
  return s;
}

/* { dg-final { scan-tree-dump-times "Choosing vector mode V16QI" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 16 byte vectors and unroll factor 64" 1 "vect" } } */
/* { dg-final { scan-assembler-times {\tsdot\t} 1 } } */
/* { dg-final { scan-assembler-times {\tudot\t} 3 } } */
