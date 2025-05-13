#ifndef HAVE_DEFINED_VX_BINARY_RUN_H
#define HAVE_DEFINED_VX_BINARY_RUN_H

int
main ()
{
  unsigned i, k;
  T out[N];

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      T x = TEST_DATA[i][0][0];
      T *in = TEST_DATA[i][1];
      T *expect = TEST_DATA[i][2];

      TEST_RUN (T, NAME, out, in, x, N);

      for (k = 0; k < N; k++)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
