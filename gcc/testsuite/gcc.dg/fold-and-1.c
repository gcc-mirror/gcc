/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
fn1 (int x, int y)
{
  int tem1 = x | y;
  int tem2 = ~(x & y);
  return tem1 & tem2;
}

int
fn2 (int x, int y)
{
  int tem1 = y | x;
  int tem2 = ~(x & y);
  return tem1 & tem2;
}

int
fn3 (int x, int y)
{
  int tem1 = x | y;
  int tem2 = ~(y & x);
  return tem1 & tem2;
}

int
fn4 (int x, int y)
{
  int tem1 = y | x;
  int tem2 = ~(y & x);
  return tem1 & tem2;
}

int
fn5 (int x, int y)
{
  int tem1 = ~(x & y);
  int tem2 = x | y;
  return tem1 & tem2;
}

int
fn6 (int x, int y)
{
  int tem1 = ~(x & y);
  int tem2 = y | x;
  return tem1 & tem2;
}

int
fn7 (int x, int y)
{
  int tem1 = ~(y & x);
  int tem2 = x | y;
  return tem1 & tem2;
}

int
fn8 (int x, int y)
{
  int tem1 = ~(y & x);
  int tem2 = y | x;
  return tem1 & tem2;
}

/* { dg-final { scan-tree-dump-not " \\| " "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\& " "cddce1" } } */
/* { dg-final { scan-tree-dump-not "~" "cddce1" } } */
