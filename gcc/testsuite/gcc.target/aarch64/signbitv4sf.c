/* { dg-do run } */
/* { dg-additional-options "-O3 --save-temps" } */

extern void abort ();

#define N 1024
float in[N] = {1.0, -1.0, -2.0, 3.0, -5.0, -8.0, 13.0, 21.0};
int out[N];

void
foo ()
{
  int i;
  for (i = 0; i < N; i++)
    out[i] = __builtin_signbit (in[i]);
}

/* { dg-final { scan-assembler-not {-2147483648} } } */
/* { dg-final { scan-assembler {\tushr\tv[0-9]+.4s, v[0-9]+.4s, 31} } } */

int
main ()
{
  int i;

  foo ();

  for (i = 0; i < N; i++)
  {
    if (in[i] >= 0.0 && out[i])
      abort ();
    if (in[i] < 0.0 && !out[i])
      abort ();
  }

  return 0;
}

