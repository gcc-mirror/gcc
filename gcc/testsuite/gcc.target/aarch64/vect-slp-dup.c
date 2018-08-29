/* { dg-do compile } */

/* { dg-options "-O3 -ftree-vectorize -fno-vect-cost-model" } */

void bar (double);

void
foo (double *restrict in, double *restrict in2,
     double *restrict out1, double *restrict out2)
{
  for (int i = 0; i < 1024; i++)
    {
      out1[i] = in[i] + 2.0 * in[i+128];
      out1[i+1] = in[i+1] + 2.0 * in2[i];
      bar (in[i]);
    }
}

/* { dg-final { scan-assembler-not "dup\tv\[0-9\]+.2d, v\[0-9\]+" } } */

