#ifndef HAVE_DEFINE_VEC_SAT_UNARY_VV_RUN_H
#define HAVE_DEFINE_VEC_SAT_UNARY_VV_RUN_H

int
main ()
{
  unsigned i, k;

  for (i = 0; i < sizeof (DATA) / sizeof (DATA[0]); i++)
    {
      T *data = &DATA[i];

      RUN_UNARY (data->out, data->in, N);

      for (k = 0; k < N; k++)
	if (data->out[k] != data->expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
