/* EXTRA_OPTIONS: -fcaller-saves */

extern char *malloc();

main()
{
  char w[1000];
  int i, j, nres, t[1000];
  float dv, (**dmat)[4][4];
  short at1, at2;

  nres = 200;

  for (i = 0; i < nres; i++)
    {
      w[i] = rand() & 15;
      t[i] = 0;
    }

  dmat = (float (**)[4][4]) malloc(nres * sizeof(*dmat));
  if (!dmat)
    return 1;
  for (i = 0; i < nres; i++)
    {
      dmat[i] = (float (*)[4][4]) malloc(nres * sizeof(**dmat));
      if (!dmat[i])
	return 1;
    }

  for (i = 0; i < nres; i++)
    for (j = i; j < nres; j++)
      for (at1 = 0; at1 <= 3; at1++)
	for (at2 = 0; at2 <= 3; at2++)
	  if (i != j || at1 != at2)
	    if ((w[i] & (1 << at1)) && (w[j] & (1 << at2)))
	      {
		dv = 20.0 * (rand() & 32767) / 32768.0;
		dmat[i][j][at1][at2] = dmat[j][i][at2][at1] = dv;
	      }
	    else
	      dmat[i][j][at1][at2] = dmat[j][i][at2][at1] = 999.0;
	  else
	    dmat[i][j][at1][at2] = 0.0;

  return 0;
}
