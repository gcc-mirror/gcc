/* PR optimization/6010 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-all-loops" } */
/* { dg-options "-O2 -funroll-all-loops -march=pentium3" { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */

void bar (float);

void foo (float y, unsigned long z)
{
  int b;
  float c = y;

  for (b = 0; b < z; b++)
    {
      bar (c);
      if (c == y)
	c = -y;
      else
	c = y;
    }
}
