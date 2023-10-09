/* PR tree-optimization/111009 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#define signbit(x) __builtin_signbit(x)

static void test(double l, double r)
{
  if (l == r && (signbit(l) || signbit(r)))
    ;
  else
    __builtin_abort();
}

int main()
{
  test(0.0, -0.0);
}

