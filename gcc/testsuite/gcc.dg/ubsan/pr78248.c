/* PR rtl-optimization/78232 */
/* PR rtl-optimization/78248 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

static struct S
{
  int f0:15;
} a;

int b[1], c = 2, d, e, f, g;

int main ()
{
  struct S h = { -2 };
  for (; e < 640; e++)
    for (; f < 1; f++)
      {
        if (c < 2)
          {
            d = b[e];
            h = a;
          }
        g = c;
        c = 1;
        if (!h.f0)
          break;
        c = g;
      }
  return 0; 
}
