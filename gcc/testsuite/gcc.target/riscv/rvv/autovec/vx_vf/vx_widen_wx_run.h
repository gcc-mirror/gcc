#ifndef HAVE_DEFINED_WX_WIDEN_RUN_H
#define HAVE_DEFINED_WX_WIDEN_RUN_H

int
main ()
{
  unsigned i, k;

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      DATA_TYPE *data = &TEST_DATA[i];
      WT *vs2 = data->vs2;
      NT rs1 = data->rs1;
      WT *expect = data->expect;
      WT *vd = data->vd;

      TEST_RUN (WT, NT, NAME, vd, vs2, rs1, N);

      for (k = 0; k < N; k++)
	if (vd[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
