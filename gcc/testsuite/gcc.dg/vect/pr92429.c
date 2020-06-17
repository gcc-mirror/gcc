/* PR92429 */
/* { dg-do compile } */
/* { dg-additional-options "-O1 -fno-tree-fre" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

void
as (int *gl, int k1)
{
  while (k1 < 1)
    {
      gl[k1] = gl[k1] * gl[k1] / 2;
      ++k1;
    }
}
