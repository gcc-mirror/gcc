/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

void
sj (int qh, int rn, int *by)
{
  for (;;)
    if (qh != 0)
      {
	int dc;

	for (dc = 0; dc < 17; ++dc)
	  {
	    int nn;

	    nn = (rn != 0) ? qh : dc;
	    if (nn != 0)
	      qh = nn;
	    else
	      qh = (qh != 0) ? *by : dc;
	  }
      }
}
