#ifndef HAVE_DEFINED_VF_BINOP_RUN_H
#define HAVE_DEFINED_VF_BINOP_RUN_H

#include <math.h>
#include <stdio.h>

#define TYPE_FABS(x, T)                                                        \
  (__builtin_types_compatible_p (T, double) ? fabs (x) : fabsf (x))

#define MAX_RELATIVE_DIFF(T)                                                   \
  (__builtin_types_compatible_p (T, _Float16) ? 0.1f  :                        \
  (__builtin_types_compatible_p (T, float)    ? 0.01f : 0.01))

int
main ()
{
  unsigned i, k;
  T out[N];

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      T f = TEST_DATA[i][0][0];
      T *in = TEST_DATA[i][1];
      T *expect = TEST_DATA[i][2];

      TEST_RUN (T, NAME, out, in, f, N);

      for (k = 0; k < N; k++)
	{
	  T diff = expect[k] - out[k];
	  if (TYPE_FABS (diff, T)
	      > MAX_RELATIVE_DIFF (T) * TYPE_FABS (expect[k], T)) {
        printf("Mismatch at i=%u, k=%u: expect=%f, out=%f, diff=%f\n", i, k, (double)expect[k], (double)out[k], (double)diff);
        __builtin_abort ();
      }
	}
    }

  return 0;
}

#endif
