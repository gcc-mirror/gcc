#ifndef HAVE_DEFINED_VEC_SAT_BINARY_VVV_RUN_H
#define HAVE_DEFINED_VEC_SAT_BINARY_VVV_RUN_H

/* To leverage this header files for run test, you need to:
   1. define T as the type, for example uint8_t,
   2. defint N as the test array size, for example 16.
   3. define RUN_VEC_SAT_BINARY as run function.
   4. prepare the test_data for test cases.
 */

int
main ()
{
  unsigned i, k;
  T out[N];

  for (i = 0; i < sizeof (test_data) / sizeof (test_data[0]); i++)
    {
      T *op_1 = test_data[i][0];
      T *op_2 = test_data[i][1];
      T *expect = test_data[i][2];

      RUN_VEC_SAT_BINARY (T, out, op_1, op_2, N);

      for (k = 0; k < N; k++)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
