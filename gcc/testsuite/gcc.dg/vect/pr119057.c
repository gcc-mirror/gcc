/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-vrp -fno-tree-forwprop" } */

int a, b, c, d;
unsigned e;
static void f(void)
{
  unsigned h;
  for (d = 0; d < 2; d++)
    b |= e;
  h = b;
  c |= h;
}
int main()
{
  for (; a; a++)
    f();
  return 0;
}
