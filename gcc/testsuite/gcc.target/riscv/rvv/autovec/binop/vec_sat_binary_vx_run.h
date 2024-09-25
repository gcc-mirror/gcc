#ifndef HAVE_DEFINED_VEC_SAT_BINARY_VX_RUN_H
#define HAVE_DEFINED_VEC_SAT_BINARY_VX_RUN_H

int
main ()
{
  unsigned i, k;
  T d;

  for (i = 0; i < sizeof (DATA) / sizeof (DATA[0]); i++)
    {
      d = DATA[i];
      RUN_BINARY_VX (&d.x[N], d.b, N);

      for (k = 0; k < N; k++)
	if (d.x[k] != d.expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
