/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize -fno-vect-cost-model" } */

struct {
  unsigned a;
  unsigned b;
} c;
int d, e, f, g, h;
int main()
{
  if (c.b && g && g > 7)
    goto i;
 j:
  if (c.a) {
    int k = 0;
    unsigned l = c.b;
    if (0) {
    m:
      k = l = c.b;
    }
    c.a = k;
    c.b = l;
  }
  if (0) {
  i:
    goto m;
  }
  if (d)
    goto j;
  for (f = 5; f; f--)
    if (h)
      e = 0;
  return 0;
}
