/* PR debug/48159 */
/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */

void
foo (double x, int y, double *__restrict z, double *__restrict w)
{
  while (y--)
    *z++ = (*w++ = 0) * x;
}
