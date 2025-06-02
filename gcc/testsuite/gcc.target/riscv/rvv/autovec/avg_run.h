#ifndef HAVE_DEFINED_AVG_RUN_H
#define HAVE_DEFINED_AVG_RUN_H

int
main ()
{
  unsigned i, k;
  NT out[N];

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      NT *a = TEST_DATA[i][0];
      NT *b = TEST_DATA[i][1];
      NT *expect = TEST_DATA[i][2];

      TEST_RUN (NT, WT, NAME, a, b, out, N);

      for (k = 0; k < N; k++)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
