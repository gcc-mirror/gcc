/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

unsigned long a, c;
unsigned b;
int d, e;
long f()
{
  unsigned long g = 0;
  for (d = 0; d < 5; d += 2)
    for (e = 0; e < 5; e += 3)
      {
	c = 4 + b;
	g = -b - b;
	b = 5 * (b << 24);
      }
  a = g;
  return 0;
}

int main()
{
  f();
  if (a)
    __builtin_abort();
  return 0;
}
