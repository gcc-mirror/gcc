/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a" { target aarch64-*-* } } */

int x, y, z;

void f(void)
{
  int t = 4;
  for (; x; x++)
    {
      if (y)
	continue;
      t = 0;
    }
  z = t;
}
