/* { dg-do run } */
/* { dg-additional-options "-O3 --save-temps" } */

extern void abort ();

#define N 8
float in[N] = {1.0, -1.0, -2.0, 3.0, -5.0, -8.0, 13.0, 21.0};
int out[N];

void
foo (int *i, float *f)
{
  i[0] = __builtin_signbit (f[0]);
  i[1] = __builtin_signbit (f[1]);
}

/* { dg-final { scan-assembler-not {-2147483648} } } */
/* { dg-final { scan-assembler {\tushr\tv[0-9]+.2s, v[0-9]+.2s, 31} } } */

int
main ()
{
  int i;

  foo (out, in);
  foo (out + 2, in + 2);
  foo (out + 4, in + 4);
  foo (out + 6, in + 6);

  for (i = 0; i < N; i++)
  {
    if (in[i] >= 0.0 && out[i])
      abort ();
    if (in[i] < 0.0 && !out[i])
      abort ();
  }

  return 0;
}

