/* PR c++/60267 */
/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

void
foo (int *a, int *b, int *c)
{
  int i;
#pragma GCC ivdep
  for (i = 0; i < 64; i++)
    a[i] = b[i] * c[i];
}

