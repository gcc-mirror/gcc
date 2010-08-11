double *ZV_entries (void);
void SubMtx_fillRowZV (int irow)
{
  double *rowvec = ZV_entries ();
  double *entries;
  int ii, ipivot, jrow, kk, m;
  int *pivotsizes;

  SubMtx_blockDiagonalInfo (&pivotsizes);

  for (jrow = ipivot = kk = 0; jrow <= irow; ipivot++)
    {
      m = pivotsizes[ipivot];
      if (jrow <= irow && irow < jrow + m)
	for (ii = jrow; ii < irow; ii++)
	  {
	    rowvec[2*ii] = entries[2*kk];
	    rowvec[2*ii+1] = entries[2*kk+1];
	  }
      jrow += m;
    }
}
