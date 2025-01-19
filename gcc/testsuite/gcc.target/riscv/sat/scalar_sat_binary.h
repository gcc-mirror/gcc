#ifndef HAVE_DEFINED_SCALAR_SAT_BINARY
#define HAVE_DEFINED_SCALAR_SAT_BINARY

/* To leverage this header files for run test, you need to:
   1. define T as the type, for example uint8_t,
   2. define RUN_SAT_BINARY as run function.
   3. prepare the test_data for test cases.
 */

int
main ()
{
  unsigned i;
  T *d;

  for (i = 0; i < sizeof (test_data) / sizeof (test_data[0]); i++)
    {
      d = test_data[i];

      if (RUN_SAT_BINARY (T, d[0], d[1]) != d[2])
	__builtin_abort ();
    }

  return 0;
}

#endif
