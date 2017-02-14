/* { dg-do compile } */
/* { dg-additional-options "-g" } */

unsigned int mu;
int pt;

void
qf (void)
{
  int gy;
  long int vz;

  for (;;)
    {
      for (gy = 0; gy < 80; ++gy)
      {
	vz = mu;
	++mu;
	pt = (vz != 0) && (pt != 0);
      }
      while (gy < 81)
	while (gy < 83)
	  {
	    vz = (vz != 0) ? 0 : mu;
	    ++gy;
	  }
      pt = vz;
      ++mu;
    }
}
