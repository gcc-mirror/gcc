/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mtune=generic" } */

void
foo (char * dest, int xcount, int ycount)
{
  int x, y;
  for (y = 0; y < ycount; y++)
    for (x = 0; x < xcount; x++)
      dest[x + y*2] = 0;
}
