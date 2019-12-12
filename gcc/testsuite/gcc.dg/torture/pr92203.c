/* { dg-do compile } */
/* { dg-additional-options "-Wno-div-by-zero" } */

unsigned long int rr;

void
cw (int z9)
{
  int m5;
  unsigned long int vz = 0;
  long int *na;

  if (z9 == 0)
    rr = 0;
  else
    {
      na = (long int *) &m5;
      for (*na = 0; *na < 1; ++*na)
	{
	  na = (long int *) &vz;
	  rr /= 0;
	}
    }

  m5 = rr / 5;
  ++vz;
  if (vz != 0)
    while (z9 < 1)
      {
	if (m5 >= 0)
	  rr += m5;

	na = (long int *) &rr;
	if (*na >= 0)
	  rr = 0;
      }
}
