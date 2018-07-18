/* { dg-do run } */

int printf (const char *, ...);

int a, b = -600, c, d[] = { 0 }, e, f, g = -1, h;
unsigned i = ~0;

int main ()
{
  for (; h < 2; h++)
    {
      if (a > 0)
	printf ("%d\n", d[b]);
      f = ~(b % i);
      c = g | (f && g) && e | b;
      a = ~(~g & b);
    }
  return 0; 
}
