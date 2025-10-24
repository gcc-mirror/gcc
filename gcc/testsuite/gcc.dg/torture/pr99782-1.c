/* { dg-compile } */
/* { dg-additional-options "-mapxf" { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* PR middle-end/99782 */

int hb;

void
w4 (__int128 uv, int ng)
{
  int vh;

  for (vh = 0; vh < 14; ++vh)
    {
      ++ng;
      hb = (hb == uv) && ng;
    }
}
