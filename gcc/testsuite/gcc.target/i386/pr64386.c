/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -mavx512bw" } */

char ac[64], bc[64], ec[64];

void fc (void)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      char e = ec[i];
      ac[i] = bc[i] ? : e;
    }
}
