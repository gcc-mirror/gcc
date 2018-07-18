#define N 100

void __attribute__ ((noipa))
foo (int *out, int *in, int step)
{
  for (int i = 0; i < N; ++i)
    {
      out[0] = in[i];
      out[1] = 2;
      out += step;
    }
}

int in[N];
int out[N * 2];

int
main (void)
{
  for (int i = 0; i < N; ++i)
    {
      in[i] = i * (i + 1);
      asm volatile ("" ::: "memory");
    }

  foo (out, in, 1);
  for (int i = 0; i < N; ++i)
    if (out[i] != in[i])
      __builtin_abort ();
  if (out[N] != 2)
    __builtin_abort ();

  foo (out + N - 1, in, -1);
  if (out[0] != in[N - 1])
    __builtin_abort ();
  for (int i = 1; i <= N; ++i)
    if (out[i] != 2)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" { target { { ! vect_no_align } && vect_int } } } } */
