/* { dg-do compile } */

int
div_and_round_double (lden_orig, hden_orig)
     int lden_orig, hden_orig;
{
  int quo[4];
  register int i;
  unsigned int  work;
  register unsigned int  carry = 0;
  int  lden = lden_orig;
  int  hden = hden_orig;
  neg_double (&lden, &hden);
  for (i = 4 - 1; i >= 0; i--)
    {
      quo[i] = work / (unsigned int ) lden;
      carry = work % (unsigned int ) lden;
    }
  return 0;
}
