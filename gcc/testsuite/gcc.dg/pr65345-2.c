/* PR c/65345 */
/* { dg-do run } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options "" } */

#define CHECK(X) if (!(X)) __builtin_abort ()

_Atomic int i = 5;
_Atomic int j = 2;

void
fn1 (int a[i = 0])
{
}

void
fn2 (int a[i += 2])
{
}

void
fn3 (int a[++i])
{
}

void
fn4 (int a[++i])
{
}

void
fn5 (int a[++i][j = 10])
{
}

void
fn6 (int a[i = 7][j--])
{
}

int
main ()
{
  int a[10];
  int aa[10][10];
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
