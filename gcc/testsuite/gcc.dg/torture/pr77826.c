/* { dg-do compile } */

void
fi(unsigned long int *v0, unsigned int ow, int q2)
{
  if (ow + q2 != 0)
    if (q2 == 1)
      {
	*v0 |= q2;
	q2 ^= *v0;
      }
}
