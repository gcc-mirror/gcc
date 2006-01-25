/* Compiler generates 64-bit stores of zero for this on some targets.
   Check there is no problem for such case.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void
foo (float *dest, int xcount, int  ycount)
{
  int x, y;

  for (y = 0; y < ycount; y++)
    for (x = 0; x < xcount; x++)
      dest[x + y] = (float) 0;
}
