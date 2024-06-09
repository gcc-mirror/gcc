/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -O3 -fno-schedule-insns -fno-schedule-insns2 -fno-vect-cost-model" } */

#define K 32
short in[2*K][K];
short coeff[K][K];
void
foo ()
{
  for (int j = 0; j < K; j++)
  {
    for (int i = 0; i < 2*K; i++)
      in[i][j] = i+j;

    for (int i = 0; i < K; i++)
      coeff[i][j] = i + 2;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmv\.v\.x} 1 } } */
