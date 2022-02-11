/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

_Bool a[80];
short b, f;
void g(short h[][8][16])
{
  for (_Bool c = 0; c < b;)
    for (_Bool d = 0; d < (_Bool)f; d = 1)
      for (short e = 0; e < 16; e++)
        a[e] = h[b][1][e];
}
