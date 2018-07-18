/* { dg-do compile } */
/* { dg-additional-options "-Ofast -ftrapv" } */

int c;
unsigned char d;
int e (unsigned char *f)
{
  int g;
  for (int a; a; a++)
    {
      for (int b = 0; b < 6; b++)
	g += __builtin_abs (f[b] - d);
      f += c;
    }
  return g;
}
