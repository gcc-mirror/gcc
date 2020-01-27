/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

char
bn (char *vu)
{
  int b6;
  char wv = 0;

  for (b6 = 0; b6 <= 64; b6 += 4)
    wv += vu[b6] + vu[b6 + 1];

  return wv;
}
