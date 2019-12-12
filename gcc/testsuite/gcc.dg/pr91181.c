/* { dg-do compile } */
/* { dg-options "-Ofast" } */
/* { dg-additional-options "-mavx" { target x86_64-*-* i?86-*-* } } */

enum { a, b, c };
float *d, *e;
int f, g, h, i;
int j()
{
  float a;
  for (; h; h++)
    {
      i = h * 4;
      a = d[i + b];
      if (a) {
	  e[i + b] = g < d[i + b] * f * a ? g : d[i + b] * f * a;
	  e[i + c] = g < d[i + c] * f * a ? g : d[i + c] * f * a;
      }
      e[i + b] = e[i + c];
    }
}
