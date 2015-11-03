/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
fn1 (int x)
{
  return 42L - (42 / x) * x;
}

long
fn2 (int x)
{
  return 42L - (42 / x) * x;
}

int
fn3 (long int x)
{
  return 42L - (42 / x) * x;
}

int
fn4 (unsigned int a, int b)
{
  return a - (unsigned) ((a / b) * b);
}

int
fn5 (int a, unsigned int b)
{
  return a - ((a / b) * b);
}

unsigned int
fn6 (int a, int b)
{
  return a - ((a / b) * b);
}

/* { dg-final { scan-tree-dump-not " / " "cddce1" } } */
/* { dg-final { scan-tree-dump-not " - " "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\+ " "cddce1" } } */
