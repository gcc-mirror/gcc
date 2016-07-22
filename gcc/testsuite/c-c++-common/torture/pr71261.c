/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

extern int a, b, c, d, e, f;
void fn1()
{
  for (int g = 0; g < d; g = 1)
    for (int h = 0; h < 8; h = h + 2)
      for (int i = h; i < h + 2; i = i + 1)
	f = a && e || c && b;
}
