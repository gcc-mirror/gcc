/* { dg-additional-options "-mcpu=neoverse-v2" { target aarch64*-*-* } } */
int a;
short b, c, e;
long d, f;
long g (long h)
{
  if (h)
    return h;
  return d;
}
void i (int h[][0][0][0])
{
  for (short j; j; j += 3)
    {
      a = g(h[1][2] ? 0 : h[1][1][1][1]);
      b = e ?: f % c;
    }
}
