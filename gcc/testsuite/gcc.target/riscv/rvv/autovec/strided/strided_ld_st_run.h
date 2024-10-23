#ifndef HAVE_DEFINE_STRIDED_LD_ST_H
#define HAVE_DEFINE_STRIDED_LD_ST_H

int
main ()
{
  unsigned i, k;

  for (i = 0; i < sizeof (DATA) / sizeof (DATA[0]); i++)
    {
      T stride = DATA[i][0][0];
      T *in = DATA[i][1];
      T *out = DATA[i][2];
      T *expect = DATA[i][3];

      RUN_STRIDED_LD_ST (out, in, stride, N / stride);

      for (k = 0; k < N; k = k + stride)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif

