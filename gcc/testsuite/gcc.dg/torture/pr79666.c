/* { dg-do run } */

struct
{
  unsigned a:6;
} b;

int c, e, g = 7;
signed char d, f = 6, h = -10;

void fn1 ()
{
  for (; c < 9; c++)
    {
      if (f)
	g = ~(~0 / (g ^ e));
      b.a = ~0;
      d = ~((h ^ b.a) & 132 & (~(f && g) | (d && 1)));
      e = ~0;
      if (d < 127 || f < 1)
	continue;
      g = 0;
    }
}

int main ()
{
  fn1 ();
  return 0; 
}
