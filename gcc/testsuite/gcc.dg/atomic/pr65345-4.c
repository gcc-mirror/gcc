/* PR c/65345 */
/* { dg-options "" } */

#define CHECK(X) if (!(X)) __builtin_abort ()

_Atomic float i = 5;
_Atomic float j = 2;

void
fn1 (float a[(int) (i = 0)])
{
}

void
fn2 (float a[(int) (i += 2)])
{
}

void
fn3 (float a[(int) ++i])
{
}

void
fn4 (float a[(int) ++i])
{
}

void
fn5 (float a[(int) ++i][(int) (j = 10)])
{
}

void
fn6 (float a[(int) (i = 7)][(int) j--])
{
}

int
main ()
{
  float a[10];
  float aa[10][10];
  fn1 (a);
  CHECK (i == 0);
  fn2 (a);
  CHECK (i == 2);
  fn3 (a);
  CHECK (i == 3);
  fn4 (a);
  CHECK (i == 4);
  fn5 (aa);
  CHECK (i == 5);
  CHECK (j == 10);
  fn6 (aa);
  CHECK (i == 7);
  CHECK (j == 9);
}
