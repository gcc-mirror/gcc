/* { dg-lto-do link } */
/* { dg-require-linker-plugin "" } */
/* { dg-lto-options { { -O -flto -fdump-tree-optimized } } } */

_Complex double bar (_Complex double x, _Complex double y);

_Complex double foo (_Complex double x, _Complex double y)
{
  return x / y;
}

volatile double r;

int main ()
{
  _Complex double x = r + 1.0iF * r;
  _Complex double y = r + 1.0iF * r;
  _Complex double z = foo (x, y);
  volatile _Complex double w = bar (z, x);
}

/* { dg-final { scan-ltrans-tree-dump-times "divdc3" 1 "optimized" } } */
