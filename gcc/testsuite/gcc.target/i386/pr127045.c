/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O1" } */

float a;
long b, c;
int d, e, f, *g, *h, **i, **l, **m;
short n, o, p, q;
void
r (short s, short t)
{
  while (-32429 - (-28412 | t) > s)
    ;
}
void
u (short s, int t, long v)
{
  h = &e;
  i = &g;
  l = &h;
  m = &g;
  if (v == 1073741824)
    {
      *i = &d;
      *l = *m;
    }
  d = -1610809352 / t;
  f = *h;
  n = f;
  o = n % 181 + 181;
  p = 83;
  q = 25 * o % 181;
  p = p + q;
  r (s - 16044, p + 28236);
}
void
w (float s)
{
  long j, k;
  a = 8.0f * s;
  b = 2.0f;
  j = a;
  j = j ^ b;
  c = -4294967294 ^ j;
  k = 2 ^ j;
  c = c & k;
  u (-16384, 24, 1073741824 ^ c);
}
int
main ()
{
  w (536870912.0f);
  return 0;
}
