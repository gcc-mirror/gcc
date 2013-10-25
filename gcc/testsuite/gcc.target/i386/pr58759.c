/* { dg-do run } */
/* { dg-options "-O2" } */


int a, b, c, d, e, f, h, l, m, n, k, o;
long long g;

struct S
{
  int f1;
  int f2;
  int f3;
  int f4;
};

static struct S i = {0,0,0,0}, j;

void
foo ()
{
  m = 1 & d;
  n = b + c;
  o = k >> 1;
  f = 0 == e;
}

int
main ()
{
  for (; h < 1; h++)
    {
      g = 1 | (0 > 1 - a ? 0 : a);
      foo ();
      for (l = 0; l < 3; l++)
      j = i;
    }
  return 0;
}
