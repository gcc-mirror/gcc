#ifndef HAVE_DEFINED_VX_TERNARY_RUN_H
#define HAVE_DEFINED_VX_TERNARY_RUN_H

int
main ()
{
  unsigned i, k;

  for (i = 0; i < sizeof (TEST_DATA) / sizeof (TEST_DATA[0]); i++)
    {
      T rs1 = TEST_DATA[i][0][0];
      T *vs2 = TEST_DATA[i][1];
      T *vd = TEST_DATA[i][2];
      T *expect = TEST_DATA[i][3];

      TEST_RUN (T, NAME, vd, vs2, rs1, N);

      for (k = 0; k < N; k++)
	if (vd[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}

#endif
