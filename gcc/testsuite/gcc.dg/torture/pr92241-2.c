/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

int ze, r2;
int i0[2];

void
np (int ch)
{
  while (ch < 1)
    {
      if (i0[ch] != 0)
        ze = r2 = ch;

      ++ch;
    }
}
