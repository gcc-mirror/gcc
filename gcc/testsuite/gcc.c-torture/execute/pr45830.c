/* PR tree-optimization/45830 */

extern void abort (void);

long long va, vb, vc, vd, ve;

__attribute__((noinline)) int
foo (int x)
{
  long long a, b, c, d, e;
  switch (x)
    {
    case 0:
    case 3:
    case 1:
    case 2:
    case 4:
      a = 1;
      b = 129;
      c = -12;
      d = -4;
      e = 128;
      break;
    case 23:
    case 26:
    case 19:
    case 65:
    case 5:
      a = 2;
      b = 138;
      c = 115;
      d = 128;
      e = -16;
      break;
    case 21:
    case 20:
    case 22:
    case 38:
    case 27:
    case 66:
    case 45:
    case 47:
      a = 3;
      b = 6;
      c = 127;
      d = 25;
      e = 257;
      break;
    default:
      a = 0;
      b = 18;
      c = 0;
      d = 64;
      e = 32768L;
      break;
    }
  va = a;
  vb = b;
  vc = c;
  vd = d;
  ve = e;
}

int
bar (int x)
{
  if (x < 0)
    return 3;
  if (x < 5)
    return 0;
  if (x == 5 || x == 19 || x == 23 | x == 26 || x == 65)
    return 1;
  if ((x >= 20 && x <= 22) || x == 27 || x == 38
      || x == 45 || x == 47 || x == 66)
    return 2;
  return 3;
}

long long expected[] =
{ 1, 129, -12, -4, 128, 2, 138, 115, 128, -16,
  3, 6, 127, 25, 257, 0, 18, 0, 64, 32768L };

int
main (void)
{
  int i, v;
  for (i = -4; i < 70; i++)
    {
      foo (i);
      v = bar (i);
      if (va != expected[5 * v] || vb != expected[5 * v + 1]
	  || vc != expected[5 * v + 2] || vd != expected[5 * v + 3]
	  || ve != expected[5 * v + 4])
	abort ();
    }
  return 0;
}
