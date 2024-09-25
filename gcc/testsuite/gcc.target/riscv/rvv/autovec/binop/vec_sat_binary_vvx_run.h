#ifndef HAVE_DEFINED_VEC_SAT_BINARY_VVX_RUN_H
#define HAVE_DEFINED_VEC_SAT_BINARY_VVX_RUN_H

int
main ()
{
  unsigned i, k;
  OUT_T out[N];

  for (i = 0; i < sizeof (expect_data) / sizeof (expect_data[0]); i++)
    {
      IN_T *op_1 = op_1_data[i];
      IN_T op_2 = op_2_data[i];
      OUT_T *expect = expect_data[i];

      RUN_VEC_SAT_BINARY (OUT_T, IN_T, out, op_1, op_2, N);

      for (k = 0; k < N; k++)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif

