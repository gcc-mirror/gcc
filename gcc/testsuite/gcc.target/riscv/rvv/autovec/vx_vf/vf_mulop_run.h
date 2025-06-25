#ifndef HAVE_DEFINED_VF_MULOP_RUN_H
#define HAVE_DEFINED_VF_MULOP_RUN_H

#include <math.h>

#define TYPE_FABS(x, T)                                                        \
  (__builtin_types_compatible_p (T, double) ? fabs (x) : fabsf (x))

int
main ()
{
  unsigned i, k;

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      T f = TEST_DATA[i][0][0];
      T *in = TEST_DATA[i][1];
      T *out = TEST_DATA[i][2];
      T *expect = TEST_DATA[i][3];

      TEST_RUN (T, NAME, out, in, f, N);

      for (k = 0; k < N; k++)
	{
	  T diff = expect[k] - out[k];
	  if (TYPE_FABS (diff, T) > .01 * TYPE_FABS (expect[k], T))
	    __builtin_abort ();
	}
    }

  return 0;
}

#endif
