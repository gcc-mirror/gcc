/* PR rtl-optimization/123114 */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-vrp" } */

volatile int a, b, g;
unsigned short e;
int f;

int
main ()
{
  int c, d;
  while (a)
    {
      c = e = b << 2;
      d = f;
      g = (c <= 0 && d) || c ? 0 : d;
      a = g;
    }
  return 0;
}
