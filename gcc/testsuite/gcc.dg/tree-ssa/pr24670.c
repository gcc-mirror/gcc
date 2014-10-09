/* { dg-do compile } */
/* { dg-options "-O2" } */

void add128 (long long int *);

__inline__ void
shift128Right (int count, long long int *z1Ptr)
{
  long long int z1;
  if (count == 0);
  else if (count < 64);
  else
    z1 = (count < 64) ? count : 0;
  *z1Ptr = z1;
}

void
float128_rem ()
{
  signed int expDiff;
  long long int aSig1;
  long long int sigMean1;
  if (-64 < expDiff)
    shift128Right (-expDiff, &aSig1);
  add128 (&sigMean1);
}
