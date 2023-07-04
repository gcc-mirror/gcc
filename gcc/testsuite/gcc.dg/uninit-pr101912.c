/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int getint (void);
int
tzloadbody (void)
{
  int n = getint ();
  int prevcorr;
  int leapcnt = 0;
  for (int i = 0; i < n; i++)
    {
      int corr = getint ();
      if (corr < 1 || (corr == 1 && !(leapcnt == 0 || (prevcorr < corr ? corr == prevcorr + 1 : (corr == prevcorr || corr == prevcorr - 1))))) /* { dg-bogus "uninitialized" "pr101912" } */
	return -1;

      prevcorr = corr;
      leapcnt++;
    }
  return leapcnt;
}
