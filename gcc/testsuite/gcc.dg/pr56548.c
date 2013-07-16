/* PR middle-end/56548 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=pentium3" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

short
foo (short x)
{
  int i;

  for (i = 0; i < 3; i++)
    if (x > 0)
      x--;

  return x;
}
