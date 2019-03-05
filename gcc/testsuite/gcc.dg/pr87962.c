/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=bdver2" { target { x86_64-*-* i?86-*-* } } } */

int a, b;

int c()
{
  long d, e;
  while (a) {
      a++;
      b = 0;
      for (; b++ - 2; d = d >> 1)
	e += d;
  }
  return e;
}
